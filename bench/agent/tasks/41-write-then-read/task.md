# Task 41-write-then-read

Write a complete Writ program (`fn main`) for this task. Output must match exactly, including the final newline.

Run it as: `writ run solution.wr --grant fs=out` (from this task's directory).

Run with `--grant fs=out` (a directory named `out` exists and is writable). Write `hello from writ` to `out.txt` inside the granted directory, read it back and print what you read. `main` must return `Result[(), IoError]` and use `?`.
