# Task 48-pipeline

Write a complete Writ program (`fn main`) for this task. Output must match exactly, including the final newline.

Run it as: `writ run solution.wr` (from this task's directory).

Build a 3-stage pipeline with two channels: stage 1 sends the words of "alpha beta gamma delta", stage 2 receives words and sends their lengths, stage 3 receives lengths and returns their total. Run all three as tasks in one group, closing channels when done. Print the total.
