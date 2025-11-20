# yaml-test-suite data

This directory vendors the generated `data/` from the upstream YAML test suite:
https://github.com/yaml/yaml-test-suite (MIT License). Only the generated
artifacts and `License` are kept; the rest of the upstream repo is omitted.

## Layout
- `data/<CASE_ID>/`: canonical files for each test case (`in.yaml`, `out.yaml`
  or `emit.yaml`, `in.json`, `test.event`, optional `error`, marker `===`).
  Some emitter cases have multiple numbered subdirs; many entries under
  `data/tags/` are symlinks pointing at the canonical cases.
- `License`: upstream license for the test-suite content.

## How we use it
`tests/testthat/test-yaml-test-suite.R` loads cases from `data/` and compares
parser output against upstream expectations, including tag metadata, JSON
round-trips, and libyaml event streams. Symlinks allow categories of cases
(e.g., `tags/`) to share canonical inputs without duplicating content.

## Regenerating
From the package root:
```
Rscript tools/regenerate-yaml-test-suite-data.R
# optionally pin a ref:
YAML_TEST_SUITE_REF=<commit-ish> Rscript tools/regenerate-yaml-test-suite-data.R
```
This clones the upstream repo into a temporary directory, runs `make data`,
replaces `tests/testthat/yaml-test-suite/data/`,
and refreshes the `License`. Commit the resulting `data/` to keep tests
deterministic and avoid network/build steps in CI.
