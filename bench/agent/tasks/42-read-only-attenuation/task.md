# Task 42-read-only-attenuation

Write a complete Vela program (`fn main`) for this task. Output must match exactly, including the final newline.

Run it as: `vela run solution.vela --grant fs=.` (from this task's directory).

Run with `--grant fs=.` (writable). Narrow the capability with `read_only()` and try to write `x.txt` through the narrowed one; print the IoError with Vela default formatting. Then check whether x.txt exists using the original capability and print true or false.
