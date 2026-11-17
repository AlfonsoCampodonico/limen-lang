# Fuzzing

Two fuzzers, written in Limen, for the two things a server reads from strangers.

- `json.lmn` mutates JSON and checks that `json.parse` never panics, and that whatever it accepts prints and reads back as the same value.
- `http.lmn` sends mutated requests to a real `http.Server` with short time limits, from 16 clients at once, and checks that the server never panics or hangs, and answers each connection with nothing or with well-formed 200, 400 or 408 responses.

```bash
limen build fuzz/json.lmn -o /tmp/fj && /tmp/fj --seed 1 -- 300000
limen build fuzz/http.lmn -o /tmp/fh && /tmp/fh --grant listen=127.0.0.1:0 --grant net=127.0.0.1 --seed 1 -- 20000
```

`--seed` makes a run repeatable; CI runs both with a new seed each time. Each ends with `N inputs, K problems` and prints the first problems it found.
