# Task 25-generic-pair

Write a complete Vela program (`fn main`) for this task. Output must match exactly, including the final newline.

Run it as: `vela run solution.vela` (from this task's directory).

Define `struct Pair[A, B] { first: A, second: B }` and a generic `fn swap[A, B](p: Pair[A, B]) -> Pair[B, A]`. Print swap(Pair { first: 1, second: "one" }) and swap(Pair { first: true, second: [2.5] }), one per line, with Vela default formatting.
