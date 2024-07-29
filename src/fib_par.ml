let n = try int_of_string Sys.argv.(1) with _ -> 40

let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

let fib_par n =
  if n > 20 then begin
    let d1 = Domain.spawn (fun _ -> fib (n - 1))
    and d2 = Domain.spawn (fun _ -> fib (n - 2)) in
    let r1 = Domain.join d1
    and r2 = Domain.join d2 in
    r1 + r2
  end else fib n

let main () =
  let r = fib_par n in
  Printf.printf "fib(%d) = %d\n%!" n r

let _ = main ()
