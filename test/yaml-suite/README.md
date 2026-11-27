# yaml-test-suite

The official YAML test suite, vendored as test data for `std/yaml`.

- Source: https://github.com/yaml/yaml-test-suite, tag `data-2022-01-17` (the `data` branch release of 17 January 2022).
- Licence: MIT, in `LICENSE` (from tag `v2022-01-17`).
- Contents: one directory per case (a four-character id). A case holds `in.yaml`, `test.event`, and `in.json` (when the YAML is valid and has a JSON form), `out.yaml` or `emit.yaml` (when given), and `error` (when the YAML is invalid). A directory with numbered subdirectories (`2G84/00`) holds several cases. The suite's `name/` and `tags/` symbolic link trees are left out.
- Used by: the `yaml-suite` test in `test/test_limen.ml`, which lists the cases allowed to fail in `yaml_suite_known_failures`. Run it alone with `LIMEN_TEST_ONLY=yaml-suite dune exec --root . test/test_limen.exe`.
