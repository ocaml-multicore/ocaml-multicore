(* TEST
  * native
    flags = "-runtime-variant=i"
*)

open Eventring

let list_ref = ref []
let total_sizes = ref 0
let total_promoted = ref 0

let alloc ts sizes =
  let size_accum = Array.fold_left (fun x y -> x + y) 0 sizes in
    total_sizes := !total_sizes + size_accum

let counter ts counter value =
  match counter with
  | EV_C_MINOR_PROMOTED -> 
    total_promoted := !total_promoted + value
  | _ -> ()

let () =
    start ();
    let cursor = create_cursor None in
    for a = 0 to 1_000_000 do
      list_ref := (Sys.opaque_identity(ref 42)) :: !list_ref
    done;
    Gc.full_major ();
    let callbacks = { 
        ev_runtime_begin = None;
        ev_runtime_end = None;
        ev_runtime_counter = Some(counter);
        ev_alloc = Some(alloc);
        ev_lifecycle = None;
        ev_lost_events = None
    } in 
    ignore(read_poll cursor callbacks None);
    Printf.printf "total_sizes: %d, total_promoted: %d\n" !total_sizes !total_promoted