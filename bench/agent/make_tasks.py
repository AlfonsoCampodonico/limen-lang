#!/usr/bin/env python3
"""Generate the agent benchmark: 50 tasks, each a prompt (tasks/NN-name/task.md),
optional data files, and the expected stdout computed here in Python
(expected/NN-name.txt), independently of the Writ implementation.

Usage: python3 bench/agent/make_tasks.py"""
import os, shutil, collections

ROOT = os.path.dirname(os.path.abspath(__file__))
TASKS, EXPECTED = os.path.join(ROOT, "tasks"), os.path.join(ROOT, "expected")


def wfloat(x):
    """Writ's Float printing."""
    if x == int(x) and abs(x) < 1e16:
        return "%.1f" % x
    return "%.15g" % x


tasks = []


def task(name, prompt, expected, files=None, grants=None):
    tasks.append((name, prompt.strip() + "\n", expected, files or {}, grants or []))


# ---------- basics ----------
task("hello", "Print exactly `Hello, Writ!`.", "Hello, Writ!\n")
task("fizzbuzz", "For i from 1 to 30, print `Fizz` if i is divisible by 3, `Buzz` if by 5, `FizzBuzz` if by both, otherwise the number. One per line.",
     "".join(("FizzBuzz" if i % 15 == 0 else "Fizz" if i % 3 == 0 else "Buzz" if i % 5 == 0 else str(i)) + "\n" for i in range(1, 31)))
task("sum-squares", "Print the sum of the squares of the integers from 1 to 100 (inclusive).", f"{sum(i*i for i in range(1,101))}\n")
task("collatz-steps", "For each n in 1..10, print `n: steps` where steps is the number of Collatz steps to reach 1 (n even -> n/2, odd -> 3n+1). 1 takes 0 steps.",
     "".join(f"{n}: {(lambda n: (lambda f: f(f, n, 0))(lambda f, x, s: s if x == 1 else f(f, x // 2 if x % 2 == 0 else 3 * x + 1, s + 1)))(n)}\n" for n in range(1, 11)))
task("primes-below-100", "Print all primes below 100 on one line, separated by single spaces.",
     " ".join(str(p) for p in range(2, 100) if all(p % d for d in range(2, p))) + "\n")
task("reverse-words", 'Given the string `"the quick brown fox jumps"`, print its words in reverse order separated by single spaces.', "jumps fox brown quick the\n")
task("count-vowels", 'Count the vowels (a, e, i, o, u, case-insensitive) in `"Programming In Writ Is Fun"` and print the count.',
     f"{sum(c in 'aeiou' for c in 'Programming In Writ Is Fun'.lower())}\n")
task("palindromes", 'For each word in ["level", "writ", "racecar", "noon", "capability"], print `word: yes` if it reads the same backwards, else `word: no`.',
     "".join(f"{w}: {'yes' if w == w[::-1] else 'no'}\n" for w in ["level", "writ", "racecar", "noon", "capability"]))
task("gcd-lcm", "Write recursive functions gcd and lcm. For each pair (12, 18), (17, 5), (100, 75), print `a b gcd lcm` on one line.",
     "".join(f"{a} {b} {__import__('math').gcd(a,b)} {a*b//__import__('math').gcd(a,b)}\n" for a, b in [(12, 18), (17, 5), (100, 75)]))
task("triangle", "Print a right triangle of `*` with 5 rows: the first row has 1 star, the last has 5.", "".join("*" * i + "\n" for i in range(1, 6)))

# ---------- collections ----------
text = "the cat and the dog and the bird"
c = collections.Counter(text.split())
task("word-frequency", f'Count the words in `"{text}"`. Print `word count` lines sorted by count descending, ties broken alphabetically.',
     "".join(f"{w} {n}\n" for w, n in sorted(c.items(), key=lambda kv: (-kv[1], kv[0]))))
nums = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
task("dedupe", f"Given {nums}, print the list with duplicates removed, keeping first occurrences, in Writ's list format (like `[1, 2]`).",
     "[" + ", ".join(str(x) for x in dict.fromkeys(nums)) + "]\n")
task("insertion-sort", f"Implement insertion sort yourself (do not use `sort()`). Sort {nums} and print the result in Writ's list format.",
     "[" + ", ".join(map(str, sorted(nums))) + "]\n")
task("binary-search", "Implement binary search over the sorted list [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]. For targets 2, 13, 29, 4 print `target: index`, using -1 when absent.",
     "".join(f"{t}: {[2,3,5,7,11,13,17,19,23,29].index(t) if t in [2,3,5,7,11,13,17,19,23,29] else -1}\n" for t in [2, 13, 29, 4]))
task("merge-sorted", "Merge the sorted lists [1, 4, 7, 10] and [2, 3, 8, 9, 11] into one sorted list without calling sort(), and print it in Writ's list format.",
     "[1, 2, 3, 4, 7, 8, 9, 10, 11]\n")
A, B = [[1, 2], [3, 4]], [[5, 6], [7, 8]]
prod = [[sum(A[i][k] * B[k][j] for k in range(2)) for j in range(2)] for i in range(2)]
task("matrix-multiply", f"Multiply the 2x2 matrices {A} and {B} (as List[List[Int]]). Print each row of the result on its own line in Writ's list format.",
     "".join("[" + ", ".join(map(str, r)) + "]\n" for r in prod))
M = [[1, 2, 3], [4, 5, 6]]
task("transpose", f"Transpose the matrix {M} and print each row of the result on its own line in Writ's list format.",
     "".join("[" + ", ".join(map(str, r)) + "]\n" for r in zip(*M)))
names = ["alice", "bob", "anna", "carl", "beth", "cara", "dan"]
g = collections.OrderedDict()
for n in names:
    g.setdefault(n[0], []).append(n)
task("group-by-letter", f"Group {names} by first letter, in order of first appearance. Print `letter: name1, name2` per group.",
     "".join(f"{k}: {', '.join(v)}\n" for k, v in g.items()))
scores = [("ana", 81), ("ben", 92), ("cy", 77), ("di", 92), ("ed", 64)]
top = sorted(scores, key=lambda s: (-s[1], s[0]))[:3]
task("top-three", f"Given the scores {scores} (name, score), print the top three as `name score` lines, highest first, ties broken alphabetically by name.",
     "".join(f"{n} {s}\n" for n, s in top))
data = [4, 8, 15, 16, 23, 42]
task("running-total", f"For {data}, print the running totals in Writ's list format.",
     "[" + ", ".join(str(sum(data[: i + 1])) for i in range(len(data))) + "]\n")

# ---------- types ----------
task("shapes", "Define an enum Shape with Circle(Float) (radius), Rect(Float, Float) and Square(Float). Using 3.0 for pi, print the area of Circle(2.0), Rect(3.0, 4.5) and Square(1.5), one per line, printed with Writ's normal Float formatting.",
     "".join(wfloat(x) + "\n" for x in [3.0 * 2.0 * 2.0, 3.0 * 4.5, 1.5 * 1.5]))
task("safe-divide", "Write `fn divide(a: Int, b: Int) -> Option[Int]` that returns None for division by zero. For (10, 2), (7, 0), (9, 3), print the result with Writ's default formatting (e.g. `Some(5)`).",
     "Some(5)\nNone\nSome(3)\n")
task("parse-results", 'Parse each of ["12", "x7", "-3", "", "40"] as an Int. Collect successes and print `ok: [list]` then `errors: n` (the number that failed).',
     "ok: [12, -3, 40]\nerrors: 2\n")
task("question-mark", 'Write `fn sum_all(xs: List[Str]) -> Result[Int, Str]` that parses every string and adds them, returning Err("bad: <s>") at the first invalid one, using the `?` operator. Print the results for ["1", "2", "3"] and ["4", "five", "6"], one per line, with Writ default formatting.',
     'Ok(6)\nErr("bad: five")\n')
task("generic-pair", "Define `struct Pair[A, B] { first: A, second: B }` and a generic `fn swap[A, B](p: Pair[A, B]) -> Pair[B, A]`. Print swap(Pair { first: 1, second: \"one\" }) and swap(Pair { first: true, second: [2.5] }), one per line, with Writ default formatting.",
     'Pair { first: "one", second: 1 }\nPair { first: [2.5], second: true }\n')
task("expr-tree", "Define a recursive enum Expr with Num(Int), Add(Expr, Expr), Mul(Expr, Expr) and Neg(Expr). Write eval. Print the value of (2 + 3) * -(4 + 1), built as an Expr.", f"{(2 + 3) * -(4 + 1)}\n")
task("tree-depth", "Define a binary tree enum (Leaf, Node(Tree, Int, Tree)). Insert 50, 30, 70, 20, 40, 60, 80, 35 into a binary search tree, then print the in-order traversal in Writ's list format and, on the next line, the height (a single node has height 1).",
     "[20, 30, 35, 40, 50, 60, 70, 80]\n4\n")
task("traffic-light", "Model a traffic light as an enum Light with Red, Green, Yellow and `fn next(l: Light) -> Light` (Red -> Green -> Yellow -> Red). Starting at Red, print 7 states, one per line.",
     "Red\nGreen\nYellow\nRed\nGreen\nYellow\nRed\n")
task("inventory", "Use `struct Item { name: Str, qty: Int }` and a mutable List[Item] with apple 3, pear 0, plum 5. Add 2 to every item's qty in place (index assignment), then remove items with qty below 3. Print the remaining items one per line as `name qty`.",
     "apple 5\nplum 7\n")
task("bank-account", "Write `fn withdraw(balance: Int, amount: Int) -> Result[Int, Str]` that fails with \"insufficient funds\" when amount > balance and \"invalid amount\" when amount <= 0. Starting from 100, apply 30, -5, 80, 70 in order: on success update the balance, on error keep it. Print each result with Writ default formatting, then `final: N`.",
     'Ok(70)\nErr("invalid amount")\nErr("insufficient funds")\nOk(0)\nfinal: 0\n')

# ---------- strings and parsing ----------
csv = "name,age,score\nann,31,88\nbo,25,92\ncid,40,75\n"
task("csv-columns", "The program contains this CSV as a string:\n\n```\n" + csv + "```\n\nPrint the average age (integer division) and the name with the highest score, as `avg_age: N` and `best: NAME`.",
     f"avg_age: {(31+25+40)//3}\nbest: bo\n")
task("calculator", 'Write a tokenizer and an evaluator for integer arithmetic with + - * / and parentheses, respecting precedence (/ is integer division). Evaluate and print, one per line: "2 + 3 * 4", "(2 + 3) * 4", "100 / 7 - 2", "2 * (3 + (4 - 1)) * 5".',
     "14\n20\n12\n60\n")
task("roman", "Write `fn to_roman(n: Int) -> Str`. Print the Roman numerals for 1994, 2024, 4, 3999 and 58, one per line.", "MCMXCIV\nMMXXIV\nIV\nMMMCMXCIX\nLVIII\n")
plain = "attack at dawn"
task("caesar", f'Encrypt "{plain}" with a Caesar shift of 3 (lowercase letters only; leave other characters unchanged) and print it; then decrypt the result and print that.',
     "".join(chr((ord(ch) - 97 + 3) % 26 + 97) if ch.isalpha() else ch for ch in plain) + "\n" + plain + "\n")
task("run-length", 'Run-length encode "aaabccddddde" as count followed by letter (e.g. "3a"), print it, then decode it back and print that.', "3a1b2c5d1e\naaabccddddde\n")
words = "Writ is a small statically typed language where code can only touch the outside world through capabilities".split()
lines, cur = [], ""
for w in words:
    if cur and len(cur) + 1 + len(w) > 30:
        lines.append(cur); cur = w
    else:
        cur = w if not cur else cur + " " + w
lines.append(cur)
task("word-wrap", "Wrap this text at width 30 (greedy: add words to a line while it stays at most 30 characters; words are separated by single spaces) and print each line:\n\n`" + " ".join(words) + "`",
     "".join(l + "\n" for l in lines))
task("template", 'Given the template "Hello {name}, you have {count} new {thing}." and the pairs name=Ada, count=3, thing=messages, replace each `{key}` with its value and print the result. Note: in Writ source a literal brace in a string is written `\\{`.',
     "Hello Ada, you have 3 new messages.\n")
task("anagrams", 'Group ["listen", "silent", "enlist", "google", "gooegl", "cat", "act", "tac", "dog"] into anagram groups. Print each group on its own line, words separated by spaces, groups ordered by their first word\'s position in the input.',
     "listen silent enlist\ngoogle gooegl\ncat act tac\ndog\n")

# ---------- capabilities ----------
poem = "the fog comes\non little cat feet\nit sits looking\n"
task("count-file", "A file `poem.txt` is in the granted directory (run with `--grant fs=.:ro`). Print `lines: N`, `words: N` and `chars: N` (bytes) for it.",
     f"lines: {poem.count(chr(10))}\nwords: {len(poem.split())}\nchars: {len(poem)}\n", files={"poem.txt": poem}, grants=["fs=.:ro"])
task("missing-file", "Run with `--grant fs=.:ro`. Try to read `nope.txt`, which does not exist. Print `missing` if the error is a Failed IoError, `denied` if it is Denied. Then try to read `../poem.txt` and print `missing` or `denied` the same way.",
     "missing\ndenied\n", grants=["fs=.:ro"])
task("write-then-read", "Run with `--grant fs=out` (a directory named `out` exists and is writable). Write `hello from writ` to `out.txt` inside the granted directory, read it back and print what you read. `main` must return `Result[(), IoError]` and use `?`.",
     "hello from writ\n", files={"out/.keep": ""}, grants=["fs=out"])
task("read-only-attenuation", "Run with `--grant fs=.` (writable). Narrow the capability with `read_only()` and try to write `x.txt` through the narrowed one; print the IoError with Writ default formatting. Then check whether x.txt exists using the original capability and print true or false.",
     'Denied("fs: this capability is read-only")\nfalse\n', grants=["fs=."])
task("net-denied", 'Run with no grants at all. Call `get` on `root.net` for "https://example.com/" and print the result with Writ default formatting.',
     'Err(Denied("net: example.com:443 is not granted"))\n')
task("authority", "Write a pure function `fn report(xs: List[Int]) -> Str` returning `min=A max=B sum=C`, and a function `fn show(out: Stdout, xs: List[Int])` that prints report(xs). Call show(root.stdout, [7, 2, 9, 4]) from main.",
     "min=2 max=9 sum=22\n")

# ---------- concurrency ----------
task("parallel-sum", "Using a `group`, start 4 tasks: task k (k = 0..3) sums the integers from k*250000+1 to (k+1)*250000. Print the four partial sums in Writ's list format, then the total on the next line.",
     "[" + ", ".join(str(sum(range(k * 250000 + 1, (k + 1) * 250000 + 1))) for k in range(4)) + "]\n" + f"{sum(range(1, 1000001))}\n")
task("parallel-map-order", "Using one task per element inside a `group`, compute the square of each number in [3, 1, 4, 1, 5, 9, 2, 6] and print the results in the original order in Writ's list format.",
     "[" + ", ".join(str(x * x) for x in [3, 1, 4, 1, 5, 9, 2, 6]) + "]\n")
task("producer-consumer", "Create a channel with capacity 3. In a group, one task sends the numbers 1..20 and closes the channel; another receives until the channel is closed and returns the sum of the even numbers. Print that sum.",
     f"{sum(x for x in range(1, 21) if x % 2 == 0)}\n")
task("pipeline", "Build a 3-stage pipeline with two channels: stage 1 sends the words of \"alpha beta gamma delta\", stage 2 receives words and sends their lengths, stage 3 receives lengths and returns their total. Run all three as tasks in one group, closing channels when done. Print the total.",
     f"{sum(len(w) for w in 'alpha beta gamma delta'.split())}\n")
task("fan-in", "Three worker tasks each send 5 messages of the form `w<k>-<i>` (k = 1..3, i = 1..5) to one shared channel. A collector receives all 15 messages. Print how many messages were received, then the received messages sorted, one per line. Close the channel only after all three workers finish (e.g. with a nested group).",
     "15\n" + "".join(f"w{k}-{i}\n" for k in range(1, 4) for i in range(1, 6)))
task("echo-pair", "Run with `--grant listen=127.0.0.1 --grant net=127.0.0.1`. Listen on 127.0.0.1 port 0 (any free port; get it with `port()`). In one group, start a server task that accepts one connection and echoes everything it reads back until the client closes, and a client task that connects, sends \"ping\", reads the reply, closes, and returns the reply. Print the reply.",
     "ping\n", grants=["listen=127.0.0.1", "net=127.0.0.1"])


def main():
    assert len(tasks) == 50, len(tasks)
    for d in (TASKS, EXPECTED):
        shutil.rmtree(d, ignore_errors=True)
        os.makedirs(d)
    for i, (name, prompt, expected, files, grants) in enumerate(tasks, 1):
        slug = f"{i:02d}-{name}"
        tdir = os.path.join(TASKS, slug)
        os.makedirs(tdir)
        cmd = "writ run solution.wr" + "".join(f" --grant {g}" for g in grants)
        header = f"# Task {slug}\n\nWrite a complete Writ program (`fn main`) for this task. Output must match exactly, including the final newline.\n"
        header += f"\nRun it as: `{cmd}` (from this task's directory).\n\n"
        open(os.path.join(tdir, "task.md"), "w").write(header + prompt)
        open(os.path.join(tdir, "grants"), "w").write("\n".join(grants) + ("\n" if grants else ""))
        for fn, content in files.items():
            path = os.path.join(tdir, fn)
            os.makedirs(os.path.dirname(path), exist_ok=True)
            open(path, "w").write(content)
        open(os.path.join(EXPECTED, slug + ".txt"), "w").write(expected)
    print(f"wrote {len(tasks)} tasks")


main()
