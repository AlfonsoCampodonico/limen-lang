# Task 29-inventory

Write a complete Tessera program (`fn main`) for this task. Output must match exactly, including the final newline.

Run it as: `tessera run solution.tsr` (from this task's directory).

Use `struct Item { name: Str, qty: Int }` and a mutable List[Item] with apple 3, pear 0, plum 5. Add 2 to every item's qty in place (index assignment), then remove items with qty below 3. Print the remaining items one per line as `name qty`.
