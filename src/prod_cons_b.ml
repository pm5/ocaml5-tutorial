let n = try int_of_string Sys.argv.(1) with _ -> 10

module Atomic_stack : sig
  type 'a t

  val make : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
end = struct
  type 'a t = {
    mutable contents : 'a list;
    mutex : Mutex.t;
    condition : Condition.t;
  }

  let make () =
    { contents = []; mutex = Mutex.create (); condition = Condition.create () }

  let push r v =
    let push' r =
      r.contents <- v :: r.contents;
      Condition.signal r.condition
    in
    Mutex.protect r.mutex (fun () -> push' r)

  let pop r =
    let rec pop' r =
      match r.contents with
      | [] ->
          Condition.wait r.condition r.mutex;
          pop' r
      | v :: lst' ->
          r.contents <- lst';
          v
    in
    Mutex.protect r.mutex (fun () -> pop' r)
end

let s = Atomic_stack.make ()

let rec producer n =
  if n = 0 then ()
  else (
    Atomic_stack.push s n;
    Format.printf "Produced %d\n%!" n;
    producer (n - 1))

let rec consumer n acc =
  if n = 0 then acc
  else
    let v = Atomic_stack.pop s in
    Format.printf "Consumed %d\n%!" v;
    consumer (n - 1) (n + acc)

let main () =
  let p = Domain.spawn (fun _ -> producer n) in
  let c = Domain.spawn (fun _ -> consumer n 0) in
  Domain.join p;
  assert (Domain.join c = n * (n + 1) / 2)

let _ = main ()
