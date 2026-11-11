# Task 42-read-only-attenuation

Write a complete Writ program (`fn main`) for this task. Output must match exactly, including the final newline.

Run it as: `writ run solution.wr --grant fs=.` (from this task's directory).

Run with `--grant fs=.` (writable). Narrow the capability with `read_only()` and try to write `x.txt` through the narrowed one; print the IoError with Writ default formatting. Then check whether x.txt exists using the original capability and print true or false.
