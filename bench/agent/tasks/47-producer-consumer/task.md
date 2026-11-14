# Task 47-producer-consumer

Write a complete Tessera program (`fn main`) for this task. Output must match exactly, including the final newline.

Run it as: `tessera run solution.tsr` (from this task's directory).

Create a channel with capacity 3. In a group, one task sends the numbers 1..20 and closes the channel; another receives until the channel is closed and returns the sum of the even numbers. Print that sum.
