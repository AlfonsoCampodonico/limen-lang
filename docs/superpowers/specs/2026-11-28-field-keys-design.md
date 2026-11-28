# Field keys: `name: T as "key"`, design

## Intent

`yaml.decode` and `json.decode` match a key to a struct field by the field's
name. Keys that aren't Limen names can't be read into a struct:
`current-context` and `certificate-authority-data` in a kubeconfig,
camelCase in JSON APIs (`resourceVersion`), odd SQL column names. Kubeconfig
support in `kube` needs the kubeconfig's keys. The user chose an explicit key
on the field, over an automatic `_`/`-` match.

## Goals

- A struct field can name its outside key: `current_context: Str as "current-context"`.
- `json.encode`/`decode`, `yaml.encode`/`decode` and `sql.decode`/`values`
  use that key in place of the field name, in both compilers and both backends.
- Inside Limen the field keeps its own name: literals, patterns, field access,
  printing and `==` are unchanged.

## Non-goals

- An attribute system (decided against for now; see ROADMAP).
- Leaving fields out, defaults for decode, renaming enum variants.

## Design

- Syntax: in a struct declaration, a field may end with `as` and a plain
  string literal (no interpolation): `name: Type as "key"`. Not on an
  embedded struct. The key is any non-empty string.
- Errors:
  - E0124 when two fields of one struct have the same outside key, counting
    a field without `as` as its own name: `a: Int as "b", b: Int` is E0124,
    "fields `a` and `b` both have the key `b`".
  - An empty key, or a string with interpolation, is a parse error.
- Where the key is used: every derived conversion that reads or writes a
  field by name (json, yaml, sql), and their error messages, which name the
  key as it appears outside ("`current-context` is missing").
- Tooling: the formatter keeps `as "key"`; the grammar accepts it; the
  language server's hover on the field shows `current_context: Str as "current-context"`.
- SPEC: the struct section, and the json, yaml and sql sections that say
  conversions match fields by name. ROADMAP: "attributes: decide before 1.0,
  once three needs exist" under 1.0, and kubeconfig no longer blocked on keys.

## Approaches

The user picked an explicit key on the field over an automatic `_`/`-`
match (which covers only hyphens and can't write the original key back).
Within that, `as "key"` reuses an existing keyword; an attribute such as
`#[key = "x"]` would start an attribute system for one use, and a string
field name (`"current-context": Str`) leaves code unable to name the field.

## Constraints

Both compilers and both backends; the self-hosted compiler stays at the
bootstrap fixed point; SPEC in the same commit; plain commit messages with no
attribution, British spelling, no em dashes; never push.
