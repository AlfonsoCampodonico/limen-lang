# Task 25-generic-pair

Write a complete Tessera program (`fn main`) for this task. Output must match exactly, including the final newline.

Run it as: `tessera run solution.tsr` (from this task's directory).

Define `struct Pair[A, B] { first: A, second: B }` and a generic `fn swap[A, B](p: Pair[A, B]) -> Pair[B, A]`. Print swap(Pair { first: 1, second: "one" }) and swap(Pair { first: true, second: [2.5] }), one per line, with Tessera default formatting.
