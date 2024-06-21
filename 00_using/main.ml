let safe_div a b = if b = 0 then None else Some (a / b)

let () =
  let test a b =
    let r = safe_div a b in
    Printf.printf "%d / %d = %s\n" a b ([%show: int option] r)
  in
  test 6 2;
  test 3 0
