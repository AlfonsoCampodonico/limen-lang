# Task 50-echo-pair

Write a complete Tessera program (`fn main`) for this task. Output must match exactly, including the final newline.

Run it as: `tessera run solution.tsr --grant listen=127.0.0.1 --grant net=127.0.0.1` (from this task's directory).

Run with `--grant listen=127.0.0.1 --grant net=127.0.0.1`. Listen on 127.0.0.1 port 0 (any free port; get it with `port()`). In one group, start a server task that accepts one connection and echoes everything it reads back until the client closes, and a client task that connects, sends "ping", reads the reply, closes, and returns the reply. Print the reply.
