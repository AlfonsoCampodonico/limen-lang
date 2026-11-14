# Task 26-expr-tree

Write a complete Tessera program (`fn main`) for this task. Output must match exactly, including the final newline.

Run it as: `tessera run solution.tsr` (from this task's directory).

Define a recursive enum Expr with Num(Int), Add(Expr, Expr), Mul(Expr, Expr) and Neg(Expr). Write eval. Print the value of (2 + 3) * -(4 + 1), built as an Expr.
