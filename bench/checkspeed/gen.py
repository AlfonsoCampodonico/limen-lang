import sys
n = int(sys.argv[1])
out = []
for i in range(n):
    out.append(f'''struct Point{i} {{
    x: Int,
    y: Int,
    label: Str,
}}

enum Shape{i} {{
    Circle{i}(Float),
    Rect{i}(Float, Float),
    Empty{i},
}}

fn area{i}(s: Shape{i}) -> Float {{
    match s {{
        Circle{i}(r) => 3.14159 * r * r,
        Rect{i}(w, h) => w * h,
        Empty{i} => 0.0,
    }}
}}

fn dist{i}(a: Point{i}, b: Point{i}) -> Int {{
    let dx = a.x - b.x
    let dy = a.y - b.y
    if dx < 0 {{ -dx + dy.abs() }} else {{ dx + dy.abs() }}
}}

fn summarize{i}(xs: List[Int], names: Map[Str, Int]) -> Str {{
    let mut total = 0
    let mut seen = map()
    for x in xs {{
        if x % 2 == 0 {{
            total = total + x
        }} else {{
            seen[x.to_str()] = x * 3
        }}
    }}
    let big = xs.filter(|v| v > 10).map(|v| v * 2).sum()
    let label = names.get("k{i}") ?? -1
    let parts = [total.to_str(), big.to_str(), label.to_str(), seen.len().to_str()]
    "{{parts.join(", ")}} for {i}"
}}

fn parse{i}(s: Str) -> Result[Int, Str] {{
    let t = s.trim()
    if t.is_empty() {{
        return Err("empty")
    }}
    let n = t.parse_int() ?? {{ return Err("not a number: {{t}}") }}
    Ok(n * {i % 97 + 1})
}}
''')
out.append('fn main(root: Root) {\n    root.stdout.print("{area0(Circle0(1.0))} {dist0(Point0 { x: 1, y: 2, label: "a" }, Point0 { x: 4, y: 6, label: "b" })} {summarize0([1, 2, 30], map())} {parse0(" 7 ")}")\n}\n')
open(sys.argv[2], 'w').write("\n".join(out))
