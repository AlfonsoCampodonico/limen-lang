#!/usr/bin/env python3
"""Check that files match a GBNF grammar (llama.cpp's grammar format).

Usage: python3 grammar/gbnf_check.py GRAMMAR FILE...           files must match
       python3 grammar/gbnf_check.py GRAMMAR --reject FILE...  files must not match

Parses the GBNF syntax llama.cpp accepts (rules, literals, character classes
with ranges and negation, groups, * + ? and {m,n}) and decides membership
with an Earley recognizer over characters, which accepts exactly when some
derivation of `root` produces the whole input, the same acceptance condition
as llama.cpp's grammar matcher.
"""
import sys

# ---------- GBNF parser ----------

class G:
    def __init__(self):
        self.rules = {}  # name -> list of alternatives; alternative = list of symbols
        self.fresh = 0

    def new(self, hint):
        self.fresh += 1
        return f"{hint}#{self.fresh}"


def unescape(s, i):
    c = s[i]
    if c != "\\":
        return c, i + 1
    e = s[i + 1]
    simple = {"n": "\n", "t": "\t", "r": "\r", "\\": "\\", '"': '"', "[": "[", "]": "]", "-": "-", "^": "^"}
    if e in simple:
        return simple[e], i + 2
    if e == "x":
        return chr(int(s[i + 2:i + 4], 16)), i + 4
    if e == "u":
        return chr(int(s[i + 2:i + 6], 16)), i + 6
    raise ValueError(f"unknown escape \\{e}")


def parse_grammar(text):
    g = G()
    # strip comments (outside literals/classes)
    lines = []
    for line in text.split("\n"):
        out, in_str, in_cls, k = [], False, False, 0
        while k < len(line):
            c = line[k]
            if c == "\\" and (in_str or in_cls):
                out.append(line[k:k + 2]); k += 2; continue
            if c == '"' and not in_cls:
                in_str = not in_str
            elif c == "[" and not in_str:
                in_cls = True
            elif c == "]" and in_cls:
                in_cls = False
            elif c == "#" and not in_str and not in_cls:
                break
            out.append(c); k += 1
        lines.append("".join(out))
    src = "\n".join(lines)
    pos = 0

    def skip(nl_ok=True):
        nonlocal pos
        while pos < len(src) and (src[pos] in " \t" or (nl_ok and src[pos] in "\r\n")):
            pos += 1

    def name():
        nonlocal pos
        start = pos
        while pos < len(src) and (src[pos].isalnum() or src[pos] in "-_"):
            pos += 1
        if start == pos:
            raise ValueError(f"expected a rule name at {src[pos:pos + 30]!r}")
        return src[start:pos]

    def at_rule_start():
        # a new rule starts with `name ::=` after a newline
        k = pos
        while k < len(src) and (src[k].isalnum() or src[k] in "-_"):
            k += 1
        j = k
        while j < len(src) and src[j] in " \t":
            j += 1
        return k > pos and src.startswith("::=", j)

    def alternatives(depth):
        nonlocal pos
        alts = [sequence(depth)]
        while True:
            skip(nl_ok=depth > 0)
            if pos < len(src) and src[pos] == "|":
                pos += 1
                alts.append(sequence(depth))
            else:
                return alts

    def sequence(depth):
        nonlocal pos
        seq = []
        while True:
            # outside groups, a newline ends the rule unless the next line continues it
            k = pos
            while k < len(src) and src[k] in " \t":
                k += 1
            if depth == 0 and k < len(src) and src[k] in "\r\n":
                j = k
                while j < len(src) and src[j] in " \t\r\n":
                    j += 1
                pos = j
                if j >= len(src) or at_rule_start() or src[j] == "|":
                    if src[j:j + 1] != "|":
                        return seq
                    pos = k  # let alternatives() see the '|'
                    skip(nl_ok=True)
                    return seq
                continue
            skip(nl_ok=depth > 0)
            if pos >= len(src) or src[pos] in "|)":
                return seq
            sym = atom(depth)
            # postfix operators
            while pos < len(src) and src[pos] in "*+?{":
                op = src[pos]
                if op == "{":
                    end = src.index("}", pos)
                    body = src[pos + 1:end]
                    pos = end + 1
                    if "," in body:
                        lo, hi = body.split(",")
                        lo, hi = int(lo or 0), (int(hi) if hi.strip() else None)
                    else:
                        lo = hi = int(body)
                    sym = repeat(sym, lo, hi)
                else:
                    pos += 1
                    sym = {"*": repeat(sym, 0, None), "+": repeat(sym, 1, None), "?": repeat(sym, 0, 1)}[op]
            seq.append(sym)

    def repeat(sym, lo, hi):
        n = g.new("rep")
        alts = []
        if hi is None:
            # n ::= sym{lo} rest ; rest ::= ε | rest sym
            rest = g.new("star")
            g.rules[rest] = [[], [("N", rest), sym]]
            alts.append([sym] * lo + [("N", rest)])
        else:
            for k in range(lo, hi + 1):
                alts.append([sym] * k)
        g.rules[n] = alts
        return ("N", n)

    def atom(depth):
        nonlocal pos
        c = src[pos]
        if c == '"':
            pos += 1
            chars = []
            while src[pos] != '"':
                ch, pos = unescape(src, pos)
                chars.append(ch)
            pos += 1
            n = g.new("lit")
            g.rules[n] = [[("C", (False, [(ch, ch)])) for ch in chars]]
            return ("N", n)
        if c == "[":
            pos += 1
            neg = False
            if src[pos] == "^":
                neg = True
                pos += 1
            ranges = []
            while src[pos] != "]":
                a, pos = unescape(src, pos)
                b = a
                if src[pos] == "-" and src[pos + 1] != "]":
                    b, pos = unescape(src, pos + 1)
                ranges.append((a, b))
            pos += 1
            return ("C", (neg, ranges))
        if c == ".":
            pos += 1
            return ("C", (True, []))
        if c == "(":
            pos += 1
            alts = alternatives(depth + 1)
            skip()
            assert src[pos] == ")", f"expected ) at {src[pos:pos + 20]!r}"
            pos += 1
            n = g.new("grp")
            g.rules[n] = alts
            return ("N", n)
        return ("N", name())

    skip()
    while pos < len(src):
        n = name()
        skip(nl_ok=False)
        assert src.startswith("::=", pos), f"expected ::= after {n}"
        pos += 3
        alts = alternatives(0)
        if n in g.rules:
            raise ValueError(f"rule {n} defined twice")
        g.rules[n] = alts
        skip()
    for n, alts in g.rules.items():
        for alt in alts:
            for kind, v in alt:
                if kind == "N" and v not in g.rules:
                    raise ValueError(f"rule {n} refers to undefined rule {v}")
    if "root" not in g.rules:
        raise ValueError("no root rule")
    return g


# ---------- Earley recognizer ----------

def matches_class(cls, ch):
    neg, ranges = cls
    inside = any(a <= ch <= b for a, b in ranges)
    return inside != neg


def nullable_set(g):
    nul = set()
    changed = True
    while changed:
        changed = False
        for n, alts in g.rules.items():
            if n in nul:
                continue
            if any(all(k == "N" and v in nul for k, v in alt) for alt in alts):
                nul.add(n)
                changed = True
    return nul


def recognize(g, text, nul):
    # item: (rule, alt index, dot, origin)
    rules = {n: [tuple(a) for a in alts] for n, alts in g.rules.items()}
    sets = [dict() for _ in range(len(text) + 1)]  # item -> None (ordered set)

    def add(i, item, agenda):
        if item not in sets[i]:
            sets[i][item] = None
            agenda.append(item)

    agenda = []
    for ai in range(len(rules["root"])):
        add(0, ("root", ai, 0, 0), agenda)
    for i in range(len(text) + 1):
        agenda = list(sets[i].keys())
        completed_here = {}  # rule -> True (origin i completions, for nullable handling)
        k = 0
        while k < len(agenda):
            rule, ai, dot, origin = agenda[k]
            k += 1
            alt = rules[rule][ai]
            if dot < len(alt):
                kind, v = alt[dot]
                if kind == "N":
                    for bj in range(len(rules[v])):
                        add(i, (v, bj, 0, i), agenda)
                    if v in nul:
                        add(i, (rule, ai, dot + 1, origin), agenda)
                else:
                    if i < len(text) and matches_class(v, text[i]):
                        sets[i + 1][(rule, ai, dot + 1, origin)] = None
            else:
                # complete: advance items in origin set waiting for `rule`
                for (r2, a2, d2, o2) in list(sets[origin].keys()):
                    alt2 = rules[r2][a2]
                    if d2 < len(alt2) and alt2[d2] == ("N", rule):
                        add(i, (r2, a2, d2 + 1, o2), agenda)
        if i < len(text) and not sets[i + 1]:
            return False, i
    ok = any(r == "root" and o == 0 and d == len(rules[r][a]) for (r, a, d, o) in sets[len(text)])
    return ok, len(text)


def line_col(text, i):
    line = text.count("\n", 0, i) + 1
    col = i - (text.rfind("\n", 0, i) + 1) + 1
    return line, col


def main():
    args = sys.argv[1:]
    if len(args) < 2:
        print(__doc__)
        sys.exit(2)
    g = parse_grammar(open(args[0]).read())
    nul = nullable_set(g)
    reject = False
    failures = 0
    files = []
    for a in args[1:]:
        if a == "--reject":
            reject = True
        else:
            files.append((a, reject))
    for path, must_reject in files:
        text = open(path).read()
        ok, at = recognize(g, text, nul)
        if ok == must_reject:
            failures += 1
            if must_reject:
                print(f"FAIL {path}: accepted, but should be rejected")
            else:
                l, c = line_col(text, at)
                print(f"FAIL {path}: rejected at {l}:{c}: {text[at:at + 30]!r}")
    print(f"{len(files) - failures}/{len(files)} as expected")
    sys.exit(1 if failures else 0)


if __name__ == "__main__":
    main()
