type dls_state = Obj.t array

let unique_value = Obj.repr (ref 0)

external get_dls_state : unit -> dls_state = "%dls_get"

external set_dls_state : dls_state -> unit =
  "caml_domain_dls_set" [@@noalloc]

let create_tls () =
  let st = Array.make 8 unique_value in
  set_dls_state st

let _ = create_tls ()

type 'a key = int * (unit -> 'a)

let key_counter = Atomic.make 0

let new_key f =
  let k = Atomic.fetch_and_add key_counter 1 in
  (k, f)

(* If necessary, grow the current domain's local state array such that [idx]
 * is a valid index in the array. *)
let maybe_grow idx =
  let st = get_dls_state () in
  let sz = Array.length st in
  if idx < sz then st
  else begin
    let rec compute_new_size s =
      if idx < s then s else compute_new_size (2 * s)
    in
    let new_sz = compute_new_size sz in
    let new_st = Array.make new_sz unique_value in
    Array.blit st 0 new_st 0 sz;
    set_dls_state new_st;
    new_st
  end

let set (idx, _init) x =
  let st = maybe_grow idx in
  (* [Sys.opaque_identity] ensures that flambda does not look at the type of
   * [x], which may be a [float] and conclude that the [st] is a float array.
   * We do not want OCaml's float array optimisation kicking in here. *)
  st.(idx) <- Obj.repr (Sys.opaque_identity x)

let get (idx, init) =
  let st = maybe_grow idx in
  let v = st.(idx) in
  if v == unique_value then
    let v' = Obj.repr (init ()) in
    st.(idx) <- (Sys.opaque_identity v');
    Obj.magic v'
  else Obj.magic v
