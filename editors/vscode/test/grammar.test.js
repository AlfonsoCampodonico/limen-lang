// Tokenizes Tessera source with VS Code's own TextMate engine and checks the
// scopes. Run: npm test (from editors/vscode).
const fs = require("fs");
const path = require("path");
const vsctm = require("vscode-textmate");
const oniguruma = require("vscode-oniguruma");

const root = path.join(__dirname, "..");
const repo = path.join(root, "..", "..");

async function loadGrammar() {
  const wasm = fs.readFileSync(require.resolve("vscode-oniguruma/release/onig.wasm"));
  await oniguruma.loadWASM(wasm.buffer.slice(wasm.byteOffset, wasm.byteOffset + wasm.byteLength));
  const registry = new vsctm.Registry({
    onigLib: Promise.resolve({
      createOnigScanner: (p) => new oniguruma.OnigScanner(p),
      createOnigString: (s) => new oniguruma.OnigString(s),
    }),
    loadGrammar: async (scope) => {
      if (scope !== "source.tessera") return null;
      const file = path.join(root, "syntaxes", "tessera.tmLanguage.json");
      return vsctm.parseRawGrammar(fs.readFileSync(file, "utf8"), file);
    },
  });
  return registry.loadGrammar("source.tessera");
}

// [text, scopes] for each token of each line; the rule stack after the last line
function tokenize(grammar, src) {
  let stack = vsctm.INITIAL;
  const lines = src.split("\n").map((line) => {
    const r = grammar.tokenizeLine(line, stack);
    stack = r.ruleStack;
    return r.tokens.map((t) => [line.slice(t.startIndex, t.endIndex), t.scopes]);
  });
  return { lines, stack };
}

let failed = 0;
let passed = 0;
function check(name, cond, detail) {
  if (cond) passed++;
  else {
    failed++;
    console.log(`FAIL ${name}${detail ? `: ${detail}` : ""}`);
  }
}

// the innermost scope of the first token whose text is `text` (after skipping `nth` earlier ones)
function scopeOf(tokens, text, nth = 0) {
  for (const line of tokens.lines)
    for (const [t, scopes] of line)
      if (t === text && nth-- === 0) return scopes[scopes.length - 1];
  return undefined;
}

(async () => {
  const grammar = await loadGrammar();

  // what each kind of token becomes
  const src = `use errors

// a comment with "quotes" and fn
pub fn fetch(net: Net, org: Str) -> Result[List[Str], IoError] {
    let resp = net.get("https://x/{org}/repos\\n")?
    let mut n = 1_000 + 2.5e3
    let p = { origin with y: 5 }
    let s = 1..<7 |> evens |> total
    let t = corner?.x ?? -1
    let tens = [1, 2].map { |x| x * 10 }
    match o {
        Some(n) if n > 10 => "big {n.to_str()}",
        None => "{xs.join("-")} and {map([("a", 1)])}",
    }
    defer out.print("bye")
    let rs = group g until clock.after(100) { g.go(|| slow(clock, 10, 1)) }
    self.x == true
}

struct Point { x: Int }
interface Shape { fn area(self) -> Float }

test "ports" for (a, b) in [(1, 2)] { assert_eq(a, b) }
bench "build" { 1 }
foreign "c" { fn open(path: Str, flags: Int32) -> Int32 }
let test = 1
pub type Pair[T] = (T, T)
`;
  const t = tokenize(grammar, src);
  const expect = [
    ["use", "keyword.other.use.tessera"],
    ["errors", "entity.name.namespace.tessera"],
    ["// a comment with \"quotes\" and fn", "comment.line.double-slash.tessera"],
    ["pub", "storage.modifier.tessera"],
    ["fn", "storage.type.function.tessera"],
    ["fetch", "entity.name.function.tessera"],
    ["Net", "support.type.capability.tessera"],
    ["Str", "support.type.builtin.tessera"],
    ["Result", "support.type.builtin.tessera"],
    ["IoError", "entity.name.type.tessera"],
    ["->", "keyword.operator.arrow.tessera"],
    ["let", "storage.type.tessera"],
    ["get", "entity.name.function.call.tessera"],
    ["https://x/", "string.quoted.double.tessera"],
    ["{", "punctuation.section.interpolation.begin.tessera"],
    ["org", "source.tessera.embedded"],
    ["\\n", "constant.character.escape.tessera"],
    ["?", "keyword.operator.option.tessera"],
    ["mut", "storage.modifier.tessera"],
    ["1_000", "constant.numeric.integer.tessera"],
    ["2.5e3", "constant.numeric.float.tessera"],
    ["with", "keyword.other.with.tessera"],
    ["..<", "keyword.operator.range.tessera"],
    ["|>", "keyword.operator.pipe.tessera"],
    ["?.", "keyword.operator.option.tessera"],
    ["??", "keyword.operator.option.tessera"],
    ["map", "entity.name.function.call.tessera"],
    ["match", "keyword.control.tessera"],
    ["Some", "support.function.variant.tessera"],
    ["if", "keyword.control.tessera"],
    ["=>", "keyword.operator.arrow.tessera"],
    ["big ", "string.quoted.double.tessera"],
    ["to_str", "entity.name.function.call.tessera"],
    ["defer", "keyword.control.tessera"],
    ["group", "keyword.control.tessera"],
    ["until", "keyword.control.tessera"],
    ["Point", "entity.name.type.tessera"],
    ["self", "variable.language.self.tessera"],
    ["true", "constant.language.boolean.tessera"],
    ["struct", "storage.type.struct.tessera"],
    ["interface", "storage.type.interface.tessera"],
    ["Shape", "entity.name.type.tessera"],
    ["area", "entity.name.function.tessera"],
    ["test", "keyword.other.test.tessera"],
    ["bench", "keyword.other.test.tessera"],
    ["foreign", "storage.modifier.foreign.tessera"],
    ["type", "storage.type.alias.tessera"],
    ["Pair", "entity.name.type.tessera"],
  ];
  for (const [text, want] of expect) {
    const got = scopeOf(t, text);
    check(`scope of ${JSON.stringify(text)}`, got === want, `got ${got}, want ${want}`);
  }
  // a string inside an interpolation is a string; `-` before it was arithmetic
  check("string in interpolation", scopeOf(t, "-", 1) === "string.quoted.double.tessera", scopeOf(t, "-", 1));
  // nested braces inside an interpolation close at the right place: the text after is a string again
  check("nested interpolation", scopeOf(t, " and ") === "string.quoted.double.tessera", scopeOf(t, " and "));
  // `test` as a plain name is not a keyword
  const lastLine = t.lines[t.lines.length - 3];
  check("test as a name", lastLine.some(([x]) => x.includes("test")) && !lastLine.some(([, s]) => s.includes("keyword.other.test.tessera")), JSON.stringify(lastLine));
  check("sample ends at top level", t.stack.equals(vsctm.INITIAL) || t.stack.depth === 1);

  // multi-line strings: quotes inside need no escape, `"""` first on a line closes
  const ml = tokenize(grammar, `fn main(c: Conn) {
    c.write("""
        HTTP/1.1 200 OK
        Server: {name} "quoted" \\{x\\}
        a "" b """ c \\
        joined
        """)
    let after = 1
}
`);
  check("multi-line opening", scopeOf(ml, '"""') === "punctuation.definition.string.begin.tessera", scopeOf(ml, '"""'));
  check("multi-line text", scopeOf(ml, "        HTTP/1.1 200 OK") === "string.quoted.triple.tessera", scopeOf(ml, "        HTTP/1.1 200 OK"));
  check("multi-line interpolation", scopeOf(ml, "name") === "source.tessera.embedded", scopeOf(ml, "name"));
  check("multi-line quotes are text", scopeOf(ml, ' "quoted" ') === "string.quoted.triple.tessera", scopeOf(ml, ' "quoted" '));
  check("multi-line escape", scopeOf(ml, "\\{") === "constant.character.escape.tessera", scopeOf(ml, "\\{"));
  check("multi-line closing brace escape", scopeOf(ml, "\\}") === "constant.character.escape.tessera", scopeOf(ml, "\\}"));
  check("multi-line three quotes mid-line are text", scopeOf(ml, '        a "" b """ c ') === "string.quoted.triple.tessera", scopeOf(ml, '        a "" b """ c '));
  check("multi-line joined line", scopeOf(ml, "\\") === "constant.character.escape.line-continuation.tessera", scopeOf(ml, "\\"));
  check("multi-line closing", scopeOf(ml, '        """') === "punctuation.definition.string.end.tessera", scopeOf(ml, '        """'));
  check("code after multi-line", scopeOf(ml, "let") === "storage.type.tessera", scopeOf(ml, "let"));
  check("multi-line ends at top level", ml.stack.depth === 1, `depth ${ml.stack.depth}`);
  // the language server's custom semantic token type and modifiers are
  // declared, and fall back to the grammar's scopes in themes without rules
  const pkg = JSON.parse(fs.readFileSync(path.join(root, "package.json"), "utf8")).contributes;
  check("semantic type capability", pkg.semanticTokenTypes.some((t) => t.id === "capability"));
  check("semantic modifiers", ["capability", "mutable"].every((m) => pkg.semanticTokenModifiers.some((t) => t.id === m)));
  const scopes = pkg.semanticTokenScopes.find((s) => s.language === "tessera").scopes;
  check("capability scope", scopes.capability[0] === "support.type.capability.tessera", JSON.stringify(scopes));
  check("namespace scope", scopes.namespace[0] === "entity.name.namespace.tessera", JSON.stringify(scopes));
  check("capability struct scope", scopes["struct.capability"][0] === "support.type.capability.tessera", JSON.stringify(scopes));

  // `""` is still an empty string, not the start of a multi-line one
  const empty = tokenize(grammar, `let e = ""\nlet f = 1\n`);
  check("empty string", scopeOf(empty, "let", 1) === "storage.type.tessera", scopeOf(empty, "let", 1));

  // every real program ends outside any string or interpolation (strings
  // may span lines, so only the end is checked)
  const dirs = ["examples", "bench", "src/std"].map((d) => path.join(repo, d));
  let files = 0;
  for (const dir of dirs) {
    if (!fs.existsSync(dir)) continue;
    for (const f of fs.readdirSync(dir).filter((f) => f.endsWith(".tsr"))) {
      const text = fs.readFileSync(path.join(dir, f), "utf8");
      const r = tokenize(grammar, text);
      files++;
      check(`${path.basename(dir)}/${f} ends outside any string`, r.stack.depth === 1, `depth ${r.stack.depth}`);
    }
  }
  check("found programs to tokenize", files > 10, `${files}`);

  console.log(`${passed}/${passed + failed} passed (${files} programs tokenized)`);
  process.exit(failed ? 1 : 0);
})();
