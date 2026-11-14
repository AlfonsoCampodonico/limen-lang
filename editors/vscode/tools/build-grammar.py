"""Writes syntaxes/tessera.tmLanguage.json and the Markdown code-block injection.
Edit this file, not the JSON: run `python3 tools/build-grammar.py`."""
import json, os

CAPS = "Root|Net|Fs|Listen|Env|Exec|Foreign|Stdout|Stdin|Clock|Rand"
ID = r"[A-Za-z_][A-Za-z0-9_]*"
LOWER = r"[a-z_][A-Za-z0-9_]*"

grammar = {
    "$schema": "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
    "name": "Tessera",
    "scopeName": "source.tessera",
    "fileTypes": ["tessera"],
    "patterns": [{"include": "#code"}],
    "repository": {
        "code": {"patterns": [
            {"include": "#comment"},
            {"include": "#string"},
            {"include": "#test-block"},
            {"include": "#foreign-block"},
            {"include": "#fn-decl"},
            {"include": "#type-decl"},
            {"include": "#alias-decl"},
            {"include": "#use"},
            {"include": "#keyword"},
            {"include": "#capability"},
            {"include": "#type"},
            {"include": "#number"},
            {"include": "#call"},
            {"include": "#field"},
            {"include": "#operator"},
            {"include": "#punctuation"},
        ]},
        "comment": {
            "match": r"//.*$",
            "name": "comment.line.double-slash.tessera",
        },
        "string": {
            "name": "string.quoted.double.tessera",
            "begin": '"', "end": '"',
            "beginCaptures": {"0": {"name": "punctuation.definition.string.begin.tessera"}},
            "endCaptures": {"0": {"name": "punctuation.definition.string.end.tessera"}},
            "patterns": [
                {"match": r"\\[ntr0\\\"{]", "name": "constant.character.escape.tessera"},
                {"match": r"\\.", "name": "invalid.illegal.escape.tessera"},
                {"include": "#interpolation"},
            ],
        },
        "interpolation": {
            "name": "meta.interpolation.tessera",
            "begin": r"\{", "end": r"\}",
            "beginCaptures": {"0": {"name": "punctuation.section.interpolation.begin.tessera"}},
            "endCaptures": {"0": {"name": "punctuation.section.interpolation.end.tessera"}},
            "contentName": "source.tessera.embedded",
            "patterns": [{"include": "#braces"}, {"include": "#code"}],
        },
        # braces nested inside an interpolation, so `{ {a: 1} }` closes at the right brace
        "braces": {
            "begin": r"\{", "end": r"\}",
            "beginCaptures": {"0": {"name": "punctuation.section.braces.begin.tessera"}},
            "endCaptures": {"0": {"name": "punctuation.section.braces.end.tessera"}},
            "patterns": [{"include": "#braces"}, {"include": "#code"}],
        },
        # `test "name"` and `bench "name"`: contextual words, keywords only before a string
        "test-block": {
            "match": r"^\s*\b(test|bench)\b(?=\s+\")",
            "captures": {"1": {"name": "keyword.other.test.tessera"}},
        },
        "foreign-block": {
            "match": r"\b(foreign)\b(?=\s+\")",
            "captures": {"1": {"name": "storage.modifier.foreign.tessera"}},
        },
        "fn-decl": {
            "match": rf"\b(fn)\s+({LOWER})",
            "captures": {
                "1": {"name": "storage.type.function.tessera"},
                "2": {"name": "entity.name.function.tessera"},
            },
        },
        "type-decl": {
            "match": rf"\b(struct|enum|interface)\s+({ID})",
            "captures": {
                "1": {"name": "storage.type.$1.tessera"},
                "2": {"name": "entity.name.type.tessera"},
            },
        },
        # `type Name = ...` at the start of a line only: `type` is a plain name elsewhere
        "alias-decl": {
            "match": rf"^\s*(?:pub\s+)?(type)\s+({ID})",
            "captures": {
                "1": {"name": "storage.type.alias.tessera"},
                "2": {"name": "entity.name.type.tessera"},
            },
        },
        "use": {
            "match": rf"\b(use)\s+({LOWER})",
            "captures": {
                "1": {"name": "keyword.other.use.tessera"},
                "2": {"name": "entity.name.namespace.tessera"},
            },
        },
        "keyword": {"patterns": [
            {"match": r"\b(if|else|match|for|in|while|return|break|continue|select|defer|group|until)\b",
             "name": "keyword.control.tessera"},
            {"match": r"\b(let|fn|struct|enum|interface)\b", "name": "storage.type.tessera"},
            {"match": r"\b(mut|pub|dyn)\b", "name": "storage.modifier.tessera"},
            {"match": r"\b(with)\b", "name": "keyword.other.with.tessera"},
            {"match": r"\b(use)\b", "name": "keyword.other.use.tessera"},
            {"match": r"\b(true|false)\b", "name": "constant.language.boolean.tessera"},
            {"match": r"\bself\b", "name": "variable.language.self.tessera"},
        ]},
        # capabilities get their own scope: authority entering a function stands out
        "capability": {
            "match": rf"\b({CAPS})\b",
            "name": "support.type.capability.tessera",
        },
        "type": {"patterns": [
            {"match": r"\b(Some|None|Ok|Err)\b", "name": "support.function.variant.tessera"},
            {"match": r"\b(Int|Int32|Float|Bool|Str|Bytes|List|Map|Option|Result|Chan|Error|Handle)\b",
             "name": "support.type.builtin.tessera"},
            {"match": r"\b[A-Z][A-Za-z0-9_]*\b", "name": "entity.name.type.tessera"},
        ]},
        "number": {"patterns": [
            {"match": r"\b\d[\d_]*(\.\d[\d_]*([eE][+-]?\d+)?|[eE][+-]?\d+)\b", "name": "constant.numeric.float.tessera"},
            {"match": r"\b\d[\d_]*\b", "name": "constant.numeric.integer.tessera"},
        ]},
        "call": {"patterns": [
            # f(...), x.f(...), and a trailing closure: xs.map { |x| ... }
            {"match": rf"\b({LOWER})(?=\s*\()", "name": "entity.name.function.call.tessera"},
            {"match": rf"(?<=\.)({LOWER})(?=\s*\{{\s*\|)", "name": "entity.name.function.call.tessera"},
        ]},
        "field": {
            "match": rf"(?<=\.)({LOWER})\b",
            "name": "variable.other.member.tessera",
        },
        "operator": {"patterns": [
            {"match": r"\|>", "name": "keyword.operator.pipe.tessera"},
            {"match": r"\?\?|\?\.|\?", "name": "keyword.operator.option.tessera"},
            {"match": r"\.\.<|\.\.", "name": "keyword.operator.range.tessera"},
            {"match": r"=>|->", "name": "keyword.operator.arrow.tessera"},
            {"match": r"==|!=|<=|>=|&&|\|\||[<>!]", "name": "keyword.operator.comparison.tessera"},
            {"match": r"[-+*/%]", "name": "keyword.operator.arithmetic.tessera"},
            {"match": r"=", "name": "keyword.operator.assignment.tessera"},
            {"match": r"\|", "name": "punctuation.definition.closure.tessera"},
        ]},
        "punctuation": {"patterns": [
            {"match": r"[,;]", "name": "punctuation.separator.tessera"},
            {"match": r"::|:", "name": "punctuation.separator.type.tessera"},
            {"match": r"\.", "name": "punctuation.accessor.tessera"},
        ]},
    },
}

markdown = {
    "scopeName": "markdown.tessera.codeblock",
    "injectionSelector": "L:text.html.markdown",
    "patterns": [{"include": "#tessera-block"}],
    "repository": {"tessera-block": {
        "begin": r"(^|\G)(\s*)(`{3,}|~{3,})\s*(?i:(tessera)(\s+[^`~]*)?$)",
        "beginCaptures": {"3": {"name": "punctuation.definition.markdown"}, "4": {"name": "fenced_code.block.language.markdown"}},
        "end": r"(^|\G)(\2|\s{0,3})(\3)\s*$",
        "endCaptures": {"3": {"name": "punctuation.definition.markdown"}},
        "name": "markup.fenced_code.block.markdown",
        "patterns": [{
            "begin": r"(^|\G)(\s*)(.*)", "while": r"(^|\G)(?!\s*([`~]{3,})\s*$)",
            "contentName": "meta.embedded.block.tessera",
            "patterns": [{"include": "source.tessera"}],
        }],
    }},
}

here = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
for name, g in [("tessera.tmLanguage.json", grammar), ("tessera.markdown.tmLanguage.json", markdown)]:
    with open(os.path.join(here, "syntaxes", name), "w") as f:
        json.dump(g, f, indent=2)
        f.write("\n")
print("wrote syntaxes/")
