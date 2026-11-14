# Task 24-question-mark

Write a complete Tessera program (`fn main`) for this task. Output must match exactly, including the final newline.

Run it as: `tessera run solution.tsr` (from this task's directory).

Write `fn sum_all(xs: List[Str]) -> Result[Int, Str]` that parses every string and adds them, returning Err("bad: <s>") at the first invalid one, using the `?` operator. Print the results for ["1", "2", "3"] and ["4", "five", "6"], one per line, with Tessera default formatting.
