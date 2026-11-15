"""Writes syntaxes/limen.tmLanguage.json and the Markdown code-block injection.
Edit this file, not the JSON: run `python3 tools/build-grammar.py`."""
import json, os

CAPS = "Root|Net|Fs|Listen|Listener|Conn|Env|Exec|Foreign|Stdout|Stderr|Stdin|Clock|Rand"
ID = r"[A-Za-z_][A-Za-z0-9_]*"
LOWER = r"[a-z_][A-Za-z0-9_]*"

grammar = {
    "$schema": "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
    "name": "Limen",
    "scopeName": "source.limen",
    "fileTypes": ["lmn"],
    "patterns": [{"include": "#code"}],
    "repository": {
        "code": {"patterns": [
            {"include": "#comment"},
            {"include": "#string-multiline"},
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
            "name": "comment.line.double-slash.limen",
        },
        "string": {
            "name": "string.quoted.double.limen",
            "begin": '"', "end": '"',
            "beginCaptures": {"0": {"name": "punctuation.definition.string.begin.limen"}},
            "endCaptures": {"0": {"name": "punctuation.definition.string.end.limen"}},
            "patterns": [
                {"match": r"\\[ntr0\\\"{}]", "name": "constant.character.escape.limen"},
                {"match": r"\\.", "name": "invalid.illegal.escape.limen"},
                {"include": "#interpolation"},
            ],
        },
        # `"""` ending its line opens a multi-line string; `"""` first on a line closes it
        "string-multiline": {
            "name": "string.quoted.triple.limen",
            "begin": r'"""(?=[ \t\r]*$)', "end": r'^[ \t]*"""',
            "beginCaptures": {"0": {"name": "punctuation.definition.string.begin.limen"}},
            "endCaptures": {"0": {"name": "punctuation.definition.string.end.limen"}},
            "patterns": [
                {"match": r"\\$", "name": "constant.character.escape.line-continuation.limen"},
                {"match": r"\\[ntr0\\\"{}]", "name": "constant.character.escape.limen"},
                {"match": r"\\.", "name": "invalid.illegal.escape.limen"},
                {"include": "#interpolation"},
            ],
        },
        "interpolation": {
            "name": "meta.interpolation.limen",
            "begin": r"\{", "end": r"\}",
            "beginCaptures": {"0": {"name": "punctuation.section.interpolation.begin.limen"}},
            "endCaptures": {"0": {"name": "punctuation.section.interpolation.end.limen"}},
            "contentName": "source.limen.embedded",
            "patterns": [{"include": "#braces"}, {"include": "#code"}],
        },
        # braces nested inside an interpolation, so `{ {a: 1} }` closes at the right brace
        "braces": {
            "begin": r"\{", "end": r"\}",
            "beginCaptures": {"0": {"name": "punctuation.section.braces.begin.limen"}},
            "endCaptures": {"0": {"name": "punctuation.section.braces.end.limen"}},
            "patterns": [{"include": "#braces"}, {"include": "#code"}],
        },
        # `test "name"` and `bench "name"`: contextual words, keywords only before a string
        "test-block": {
            "match": r"^\s*\b(test|bench)\b(?=\s+\")",
            "captures": {"1": {"name": "keyword.other.test.limen"}},
        },
        "foreign-block": {
            "match": r"\b(foreign)\b(?=\s+\")",
            "captures": {"1": {"name": "storage.modifier.foreign.limen"}},
        },
        "fn-decl": {
            "match": rf"\b(fn)\s+({LOWER})",
            "captures": {
                "1": {"name": "storage.type.function.limen"},
                "2": {"name": "entity.name.function.limen"},
            },
        },
        "type-decl": {
            "match": rf"\b(struct|enum|interface)\s+({ID})",
            "captures": {
                "1": {"name": "storage.type.$1.limen"},
                "2": {"name": "entity.name.type.limen"},
            },
        },
        # `type Name = ...` at the start of a line only: `type` is a plain name elsewhere
        "alias-decl": {
            "match": rf"^\s*(?:pub\s+)?(type)\s+({ID})",
            "captures": {
                "1": {"name": "storage.type.alias.limen"},
                "2": {"name": "entity.name.type.limen"},
            },
        },
        "use": {
            "match": rf"\b(use)\s+({LOWER})",
            "captures": {
                "1": {"name": "keyword.other.use.limen"},
                "2": {"name": "entity.name.namespace.limen"},
            },
        },
        "keyword": {"patterns": [
            {"match": r"\b(if|else|match|for|in|while|return|break|continue|select|defer|group|until)\b",
             "name": "keyword.control.limen"},
            {"match": r"\b(let|fn|struct|enum|interface)\b", "name": "storage.type.limen"},
            {"match": r"\b(mut|pub|dyn)\b", "name": "storage.modifier.limen"},
            {"match": r"\b(with|as)\b", "name": "keyword.other.with.limen"},
            {"match": r"\b(virtual)\b", "name": "storage.modifier.limen"},
            {"match": r"\b(use)\b", "name": "keyword.other.use.limen"},
            {"match": r"\b(true|false)\b", "name": "constant.language.boolean.limen"},
            {"match": r"\bself\b", "name": "variable.language.self.limen"},
        ]},
        # capabilities get their own scope: authority entering a function stands out
        "capability": {
            "match": rf"\b({CAPS})\b",
            "name": "support.type.capability.limen",
        },
        "type": {"patterns": [
            {"match": r"\b(Some|None|Ok|Err)\b", "name": "support.function.variant.limen"},
            {"match": r"\b(Int|Int32|Float|Bool|Str|Bytes|List|Map|Option|Result|Chan|Error|Handle)\b",
             "name": "support.type.builtin.limen"},
            {"match": r"\b[A-Z][A-Za-z0-9_]*\b", "name": "entity.name.type.limen"},
        ]},
        "number": {"patterns": [
            {"match": r"\b\d[\d_]*(\.\d[\d_]*([eE][+-]?\d+)?|[eE][+-]?\d+)\b", "name": "constant.numeric.float.limen"},
            {"match": r"\b\d[\d_]*\b", "name": "constant.numeric.integer.limen"},
        ]},
        "call": {"patterns": [
            # f(...), x.f(...), and a trailing closure: xs.map { |x| ... }
            {"match": rf"\b({LOWER})(?=\s*\()", "name": "entity.name.function.call.limen"},
            {"match": rf"(?<=\.)({LOWER})(?=\s*\{{\s*\|)", "name": "entity.name.function.call.limen"},
        ]},
        "field": {
            "match": rf"(?<=\.)({LOWER})\b",
            "name": "variable.other.member.limen",
        },
        "operator": {"patterns": [
            {"match": r"\|>", "name": "keyword.operator.pipe.limen"},
            {"match": r"\?\?|\?\.|\?", "name": "keyword.operator.option.limen"},
            {"match": r"\.\.<|\.\.", "name": "keyword.operator.range.limen"},
            {"match": r"=>|->", "name": "keyword.operator.arrow.limen"},
            {"match": r"<<|>>", "name": "keyword.operator.bitwise.limen"},
            {"match": r"==|!=|<=|>=|&&|\|\||[<>!]", "name": "keyword.operator.comparison.limen"},
            {"match": r"[-+*/%]", "name": "keyword.operator.arithmetic.limen"},
            {"match": r"[&^~]", "name": "keyword.operator.bitwise.limen"},
            {"match": r"=", "name": "keyword.operator.assignment.limen"},
            {"match": r"\|", "name": "punctuation.definition.closure.limen"},
        ]},
        "punctuation": {"patterns": [
            {"match": r"[,;]", "name": "punctuation.separator.limen"},
            {"match": r"::|:", "name": "punctuation.separator.type.limen"},
            {"match": r"\.", "name": "punctuation.accessor.limen"},
        ]},
    },
}

markdown = {
    "scopeName": "markdown.limen.codeblock",
    "injectionSelector": "L:text.html.markdown",
    "patterns": [{"include": "#limen-block"}],
    "repository": {"limen-block": {
        "begin": r"(^|\G)(\s*)(`{3,}|~{3,})\s*(?i:(limen)(\s+[^`~]*)?$)",
        "beginCaptures": {"3": {"name": "punctuation.definition.markdown"}, "4": {"name": "fenced_code.block.language.markdown"}},
        "end": r"(^|\G)(\2|\s{0,3})(\3)\s*$",
        "endCaptures": {"3": {"name": "punctuation.definition.markdown"}},
        "name": "markup.fenced_code.block.markdown",
        "patterns": [{
            "begin": r"(^|\G)(\s*)(.*)", "while": r"(^|\G)(?!\s*([`~]{3,})\s*$)",
            "contentName": "meta.embedded.block.limen",
            "patterns": [{"include": "source.limen"}],
        }],
    }},
}

here = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
for name, g in [("limen.tmLanguage.json", grammar), ("limen.markdown.tmLanguage.json", markdown)]:
    with open(os.path.join(here, "syntaxes", name), "w") as f:
        json.dump(g, f, indent=2)
        f.write("\n")
print("wrote syntaxes/")
