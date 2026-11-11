# Task 40-missing-file

Write a complete Vela program (`fn main`) for this task. Output must match exactly, including the final newline.

Run it as: `vela run solution.vela --grant fs=.:ro` (from this task's directory).

Run with `--grant fs=.:ro`. Try to read `nope.txt`, which does not exist. Print `missing` if the error is a Failed IoError, `denied` if it is Denied. Then try to read `../poem.txt` and print `missing` or `denied` the same way.
