type 'a t

external make : 'a -> 'a t = "%makemutable"
external get : 'a t -> 'a = "%atomic_load"
(* currently can not exchange if the contents are a block that could be in a foreign minor heap *)
external exchange_unsafe : 'a t -> 'a -> 'a = "%atomic_exchange"
external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas"
external fetch_and_add : int t -> int -> int = "%atomic_fetch_add"

let set r x =
  exchange_unsafe r x |> ignore
let rec exchange r x =
  (* this is a workaround driven by:
       https://github.com/ocaml-multicore/ocaml-multicore/issues/257
  *)
  (* we need the read barrier induced by the get to make the exchange safe *)
  let y = get r in
  if compare_and_set r y x
  then y
  else exchange r x
let incr r =
  fetch_and_add r 1 |> ignore
let decr r =
  fetch_and_add r (-1) |> ignore
