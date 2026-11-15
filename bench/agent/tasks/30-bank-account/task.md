# Task 30-bank-account

Write a complete Limen program (`fn main`) for this task. Output must match exactly, including the final newline.

Run it as: `limen run solution.lmn` (from this task's directory).

Write `fn withdraw(balance: Int, amount: Int) -> Result[Int, Str]` that fails with "insufficient funds" when amount > balance and "invalid amount" when amount <= 0. Starting from 100, apply 30, -5, 80, 70 in order: on success update the balance, on error keep it. Print each result with Limen default formatting, then `final: N`.
