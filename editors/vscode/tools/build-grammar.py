"""Writes syntaxes/vela.tmLanguage.json and the Markdown code-block injection.
Edit this file, not the JSON: run `python3 tools/build-grammar.py`."""
import json, os

CAPS = "Root|Net|Fs|Listen|Env|Exec|Foreign|Stdout|Stdin|Clock|Rand"
ID = r"[A-Za-z_][A-Za-z0-9_]*"
LOWER = r"[a-z_][A-Za-z0-9_]*"

grammar = {
    "$schema": "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
    "name": "Vela",
    "scopeName": "source.vela",
    "fileTypes": ["vela"],
    "patterns": [{"include": "#code"}],
    "repository": {
        "code": {"patterns": [
            {"include": "#comment"},
            {"include": "#string"},
            {"include": "#test-block"},
            {"include": "#foreign-block"},
            {"include": "#fn-decl"},
            {"include": "#type-decl"},
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
            "name": "comment.line.double-slash.vela",
        },
        "string": {
            "name": "string.quoted.double.vela",
            "begin": '"', "end": '"',
            "beginCaptures": {"0": {"name": "punctuation.definition.string.begin.vela"}},
            "endCaptures": {"0": {"name": "punctuation.definition.string.end.vela"}},
            "patterns": [
                {"match": r"\\[ntr0\\\"{]", "name": "constant.character.escape.vela"},
                {"match": r"\\.", "name": "invalid.illegal.escape.vela"},
                {"include": "#interpolation"},
            ],
        },
        "interpolation": {
            "name": "meta.interpolation.vela",
            "begin": r"\{", "end": r"\}",
            "beginCaptures": {"0": {"name": "punctuation.section.interpolation.begin.vela"}},
            "endCaptures": {"0": {"name": "punctuation.section.interpolation.end.vela"}},
            "contentName": "source.vela.embedded",
            "patterns": [{"include": "#braces"}, {"include": "#code"}],
        },
        # braces nested inside an interpolation, so `{ {a: 1} }` closes at the right brace
        "braces": {
            "begin": r"\{", "end": r"\}",
            "beginCaptures": {"0": {"name": "punctuation.section.braces.begin.vela"}},
            "endCaptures": {"0": {"name": "punctuation.section.braces.end.vela"}},
            "patterns": [{"include": "#braces"}, {"include": "#code"}],
        },
        # `test "name"` and `bench "name"`: contextual words, keywords only before a string
        "test-block": {
            "match": r"^\s*\b(test|bench)\b(?=\s+\")",
            "captures": {"1": {"name": "keyword.other.test.vela"}},
        },
        "foreign-block": {
            "match": r"\b(foreign)\b(?=\s+\")",
            "captures": {"1": {"name": "storage.modifier.foreign.vela"}},
        },
        "fn-decl": {
            "match": rf"\b(fn)\s+({LOWER})",
            "captures": {
                "1": {"name": "storage.type.function.vela"},
                "2": {"name": "entity.name.function.vela"},
            },
        },
        "type-decl": {
            "match": rf"\b(struct|enum|interface)\s+({ID})",
            "captures": {
                "1": {"name": "storage.type.$1.vela"},
                "2": {"name": "entity.name.type.vela"},
            },
        },
        "use": {
            "match": rf"\b(use)\s+({LOWER})",
            "captures": {
                "1": {"name": "keyword.other.use.vela"},
                "2": {"name": "entity.name.namespace.vela"},
            },
        },
        "keyword": {"patterns": [
            {"match": r"\b(if|else|match|for|in|while|return|break|continue|select|defer|group|until)\b",
             "name": "keyword.control.vela"},
            {"match": r"\b(let|fn|struct|enum|interface)\b", "name": "storage.type.vela"},
            {"match": r"\b(mut|pub|dyn)\b", "name": "storage.modifier.vela"},
            {"match": r"\b(with)\b", "name": "keyword.other.with.vela"},
            {"match": r"\b(use)\b", "name": "keyword.other.use.vela"},
            {"match": r"\b(true|false)\b", "name": "constant.language.boolean.vela"},
            {"match": r"\bself\b", "name": "variable.language.self.vela"},
        ]},
        # capabilities get their own scope: authority entering a function stands out
        "capability": {
            "match": rf"\b({CAPS})\b",
            "name": "support.type.capability.vela",
        },
        "type": {"patterns": [
            {"match": r"\b(Some|None|Ok|Err)\b", "name": "support.function.variant.vela"},
            {"match": r"\b(Int|Int32|Float|Bool|Str|Bytes|List|Map|Option|Result|Chan|Error|Handle)\b",
             "name": "support.type.builtin.vela"},
            {"match": r"\b[A-Z][A-Za-z0-9_]*\b", "name": "entity.name.type.vela"},
        ]},
        "number": {"patterns": [
            {"match": r"\b\d[\d_]*(\.\d[\d_]*([eE][+-]?\d+)?|[eE][+-]?\d+)\b", "name": "constant.numeric.float.vela"},
            {"match": r"\b\d[\d_]*\b", "name": "constant.numeric.integer.vela"},
        ]},
        "call": {"patterns": [
            # f(...), x.f(...), and a trailing closure: xs.map { |x| ... }
            {"match": rf"\b({LOWER})(?=\s*\()", "name": "entity.name.function.call.vela"},
            {"match": rf"(?<=\.)({LOWER})(?=\s*\{{\s*\|)", "name": "entity.name.function.call.vela"},
        ]},
        "field": {
            "match": rf"(?<=\.)({LOWER})\b",
            "name": "variable.other.member.vela",
        },
        "operator": {"patterns": [
            {"match": r"\|>", "name": "keyword.operator.pipe.vela"},
            {"match": r"\?\?|\?\.|\?", "name": "keyword.operator.option.vela"},
            {"match": r"\.\.<|\.\.", "name": "keyword.operator.range.vela"},
            {"match": r"=>|->", "name": "keyword.operator.arrow.vela"},
            {"match": r"==|!=|<=|>=|&&|\|\||[<>!]", "name": "keyword.operator.comparison.vela"},
            {"match": r"[-+*/%]", "name": "keyword.operator.arithmetic.vela"},
            {"match": r"=", "name": "keyword.operator.assignment.vela"},
            {"match": r"\|", "name": "punctuation.definition.closure.vela"},
        ]},
        "punctuation": {"patterns": [
            {"match": r"[,;]", "name": "punctuation.separator.vela"},
            {"match": r"::|:", "name": "punctuation.separator.type.vela"},
            {"match": r"\.", "name": "punctuation.accessor.vela"},
        ]},
    },
}

markdown = {
    "scopeName": "markdown.vela.codeblock",
    "injectionSelector": "L:text.html.markdown",
    "patterns": [{"include": "#vela-block"}],
    "repository": {"vela-block": {
        "begin": r"(^|\G)(\s*)(`{3,}|~{3,})\s*(?i:(vela)(\s+[^`~]*)?$)",
        "beginCaptures": {"3": {"name": "punctuation.definition.markdown"}, "4": {"name": "fenced_code.block.language.markdown"}},
        "end": r"(^|\G)(\2|\s{0,3})(\3)\s*$",
        "endCaptures": {"3": {"name": "punctuation.definition.markdown"}},
        "name": "markup.fenced_code.block.markdown",
        "patterns": [{
            "begin": r"(^|\G)(\s*)(.*)", "while": r"(^|\G)(?!\s*([`~]{3,})\s*$)",
            "contentName": "meta.embedded.block.vela",
            "patterns": [{"include": "source.vela"}],
        }],
    }},
}

here = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
for name, g in [("vela.tmLanguage.json", grammar), ("vela.markdown.tmLanguage.json", markdown)]:
    with open(os.path.join(here, "syntaxes", name), "w") as f:
        json.dump(g, f, indent=2)
        f.write("\n")
print("wrote syntaxes/")
