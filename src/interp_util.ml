(* Small string helpers shared by the interpreter and the capability code. *)
let find_sub s sub from =
  let n = String.length s and m = String.length sub in
  let rec go i = if i + m > n then None else if String.sub s i m = sub then Some i else go (i + 1) in
  if m = 0 then Some from else go from
