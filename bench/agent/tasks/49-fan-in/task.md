# Task 49-fan-in

Write a complete Tessera program (`fn main`) for this task. Output must match exactly, including the final newline.

Run it as: `tessera run solution.tsr` (from this task's directory).

Three worker tasks each send 5 messages of the form `w<k>-<i>` (k = 1..3, i = 1..5) to one shared channel. A collector receives all 15 messages. Print how many messages were received, then the received messages sorted, one per line. Close the channel only after all three workers finish (e.g. with a nested group).
