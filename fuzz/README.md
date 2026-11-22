# Fuzzing

Fuzzers for what a program reads from strangers, and for the compiler, which must answer any malformed program with an ordinary diagnostic. Each takes a seed and a count, and ends with `N inputs, ..., K problems`, printing the first problems it found; `--seed` makes a run repeatable, and CI runs each with a new seed every time.

The runtime's readers, written in Limen:

- `json.lmn` mutates JSON and checks that `json.parse` never panics, and that whatever it accepts prints and reads back as the same value.
- `http.lmn` sends mutated requests to a real `http.Server` with short time limits, from 16 clients at once, and checks that the server never panics or hangs, and answers each connection with nothing or with well-formed 200, 400 or 408 responses.
- `formats.lmn` mutates URLs, query strings, CSV, base64, hex, UUIDs, ISO 8601 times, Unicode text and numbers. Nothing may panic or take long; whatever a reader accepts must print back to text it reads as the same value (`url.parse(u.to_str()) == Ok(u)`, `csv.parse(csv.write(rows))`, base64 and hex that decode only from their one encoding), and values made at random must survive the trip through text.
- `regexes.lmn` compiles mutated patterns and searches random text with them. Nothing may panic or take long, and the results must hold together: `matches` agrees with the pattern between `\A` and `\z`, a match's offsets name its text, `find_all` gives matches in order without overlaps, `split` and `replace` put the text back together, an escaped string matches itself, and `(?:p)`, `(p)` and `p|p` find what `p` finds.
- `inflate.lmn` compresses data at random levels and damages the result: gzip, zlib and deflate must give back what went in and refuse a maximum one byte short, damaged streams must give an error or data within the maximum, and `fold_gunzip` must agree with `gunzip`. With a grant of `test/zoneinfo`, it damages TZif zone files too, which must give a zone or an error.
- `ws/frames.lmn` upgrades connections to a WebSocket server that echoes, and sends masked frames in random sizes and fragments, with pings and a close. Untouched, every message must come back whole and every ping get its pong; damaged, the server must never panic or hang and must send only well-formed frames; a message in 200,000 one-byte fragments must not take long. It is in a directory of its own, where `use http` is the standard library's and not `http.lmn`.

```bash
limen build fuzz/json.lmn -o /tmp/fj && /tmp/fj --seed 1 -- 300000
limen build fuzz/http.lmn -o /tmp/fh && /tmp/fh --grant listen=127.0.0.1:0 --grant net=127.0.0.1 --seed 1 -- 20000
limen build fuzz/formats.lmn -o /tmp/ff && /tmp/ff --seed 1 -- 1000000
limen build fuzz/regexes.lmn -o /tmp/fr && /tmp/fr --seed 1 -- 20000
limen build fuzz/inflate.lmn -o /tmp/fi && /tmp/fi --grant fs=test/zoneinfo:ro --seed 1 -- 400000
limen build fuzz/ws/frames.lmn -o /tmp/fw && /tmp/fw --grant listen=127.0.0.1:0 --grant net=127.0.0.1 --seed 1 -- 16000
```

With `LIMEN_LEAKCHECK=1`, a native fuzzer also says at exit how many objects it left alive, which should be none.

The front end, in OCaml: `front/fuzz_front.ml` mutates real programs (the `.lmn` files under the paths it is given) with byte flips, token deletion, duplication and swaps, names and literals swapped for others from the corpus, fragments of other programs spliced in, deep nesting, huge literals and odd Unicode. Each mutant goes through the lexer, the parser and the checker (with the module loader and the capability checks, as `limen check` runs them) in the same process: any exception other than the lexer's and parser's own diagnostics, a diagnostic code that is not E or W and four digits, or an input that takes longer than `--time` seconds (10) is a problem. Each mutant is written into a copy of its program's directory, so the modules and files it uses are still there.

With `--selfhost` naming the self-hosted compiler, each mutant also goes to its `check`: a crash, a hang, or a disagreement with the OCaml checker on whether the program has errors is a problem. With `--exact` naming `limen` as well, any difference between what the two compilers print is counted and shown, apart from the problems. `--save DIR` keeps each problem's input, and `--replay` checks the files named, as they are.

```bash
dune build ./fuzz/front/fuzz_front.exe
limen build selfhost/limen.lmn -o /tmp/tsc
_build/default/fuzz/front/fuzz_front.exe --seed 1 --count 10000 --selfhost /tmp/tsc --exact _build/default/bin/main.exe \
  examples selfhost/tests fuzz src/std bench ../limen-book/examples
```
