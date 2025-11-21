use crate::unwind::{run_with_unwind_protect, EvalError};
use crate::{api_other, Fallible};
use extendr_api::prelude::*;
use saphyr::Tag;
use std::collections::HashMap;
use std::mem;

#[allow(improper_ctypes)]
extern "C" {
    fn Rf_eval(expr: extendr_ffi::SEXP, env: extendr_ffi::SEXP) -> extendr_ffi::SEXP;
    fn Rf_lang2(x1: extendr_ffi::SEXP, x2: extendr_ffi::SEXP) -> extendr_ffi::SEXP;
}

const HASHMAP_MIN_LEN: usize = 8;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
struct HandlerKey<'a> {
    handle: &'a str,
    suffix: &'a str,
}

impl HandlerKey<'_> {
    fn matches(&self, key: TagKeyRef<'_>) -> bool {
        self.handle == key.handle && self.suffix == key.suffix
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
struct TagKeyRef<'a> {
    handle: &'a str,
    suffix: &'a str,
}

impl<'a> From<&'a Tag> for TagKeyRef<'a> {
    fn from(tag: &'a Tag) -> Self {
        Self {
            handle: tag.handle.as_str(),
            suffix: tag.suffix.as_str(),
        }
    }
}

struct HandlerEntry<'a> {
    key: HandlerKey<'a>,
    handler: Function,
}

enum HandlerStore<'a> {
    Small(Vec<HandlerEntry<'a>>),
    Large(HashMap<HandlerKey<'a>, Function>),
}

pub(crate) struct HandlerRegistry<'a> {
    store: HandlerStore<'a>,
}

impl<'a> HandlerRegistry<'a> {
    pub(crate) fn from_robj(handlers: &'a Robj) -> Fallible<Option<Self>> {
        if handlers.is_null() {
            return Ok(None);
        }

        let list: List = handlers
            .try_into()
            .map_err(|_| api_other("`handlers` must be a named list of functions"))?;

        if list.is_empty() {
            return Ok(None);
        }

        let Some(names_attr) = list.names() else {
            return Err(api_other("`handlers` must be a named list of functions"));
        };

        let use_hash_map = list.len() >= HASHMAP_MIN_LEN;
        if use_hash_map {
            let mut handlers_map = HashMap::with_capacity(list.len());
            for (name, value) in names_attr.zip(list.values()) {
                if name.is_na() || name.is_empty() {
                    return Err(api_other("`handlers` must be a named list of functions"));
                }
                let name_str = name;
                let key = parse_handler_name(name_str)?;
                let func = value.as_function().ok_or_else(|| {
                    api_other(format!(
                        "Handler `{name}` must be a function (closure or primitive)"
                    ))
                })?;
                handlers_map.insert(key, func);
            }
            return Ok(Some(Self {
                store: HandlerStore::Large(handlers_map),
            }));
        }

        let mut entries: Vec<HandlerEntry<'a>> = Vec::with_capacity(list.len());
        for (name, value) in names_attr.zip(list.values()) {
            if name.is_na() || name.is_empty() {
                return Err(api_other("`handlers` must be a named list of functions"));
            }
            let name_str = name;
            let key = parse_handler_name(name_str)?;
            let func = value.as_function().ok_or_else(|| {
                api_other(format!(
                    "Handler `{name}` must be a function (closure or primitive)"
                ))
            })?;
            if let Some(existing) = entries.iter_mut().find(|entry| entry.key == key) {
                existing.handler = func;
            } else {
                entries.push(HandlerEntry { key, handler: func });
            }
        }

        Ok(Some(Self {
            store: HandlerStore::Small(entries),
        }))
    }

    pub(crate) fn get_for_tag(&self, tag: &Tag) -> Option<&Function> {
        let key_ref = TagKeyRef::from(tag);
        match &self.store {
            HandlerStore::Small(entries) => entries
                .iter()
                .find(|entry| entry.key.matches(key_ref))
                .map(|entry| &entry.handler),
            HandlerStore::Large(map) => {
                let lookup_key = HandlerKey {
                    handle: key_ref.handle,
                    suffix: key_ref.suffix,
                };
                // SAFETY: lookup_key only lives for this call; HashMap::get does not
                // store the reference, so widening the lifetime for lookup is sound.
                let lookup_key: &HandlerKey<'a> = unsafe { mem::transmute(&lookup_key) };
                map.get(lookup_key)
            }
        }
    }

    pub(crate) fn apply(&self, handler: &Function, arg: Robj) -> Fallible<Robj> {
        struct CallState<'a> {
            handler: &'a Function,
            arg: Robj,
            result: Option<Robj>,
        }

        let mut state = CallState {
            handler,
            arg,
            result: None,
        };
        let state_ptr = &mut state as *mut CallState;

        let protect_result = run_with_unwind_protect(|| unsafe {
            let st = &mut *state_ptr;
            // Build a call of the form handler(arg) as a language object.
            let call = Robj::from_sexp(Rf_lang2(st.handler.get(), st.arg.get()));
            let env = st.handler.environment().unwrap_or_else(global_env);
            let out = Rf_eval(call.get(), env.get());
            st.result = Some(Robj::from_sexp(out));
        });

        match protect_result {
            Ok(()) => state
                .result
                .ok_or_else(|| api_other("Handler evaluation failed unexpectedly")),
            Err(token) => Err(EvalError::Jump(token)),
        }
    }
}

fn parse_handler_name<'a>(name: &'a str) -> Fallible<HandlerKey<'a>> {
    if let Some((handle, suffix)) = split_tag_name(name) {
        Ok(HandlerKey { handle, suffix })
    } else {
        Err(api_other("`handlers` names must be valid YAML tag strings"))
    }
}

fn split_tag_name(name: &str) -> Option<(&str, &str)> {
    if let Some(pos) = name.rfind('!') {
        if pos + 1 < name.len() {
            let (handle, suffix) = name.split_at(pos + 1);
            return Some((handle, suffix));
        }
    }
    if let Some(pos) = name.rfind(':') {
        if pos + 1 < name.len() {
            let (handle, suffix) = name.split_at(pos + 1);
            return Some((handle, suffix));
        }
    }
    None
}
