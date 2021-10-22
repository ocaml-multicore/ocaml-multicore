(* TEST
  * native
    flags = "-runtime-variant=i"
*)

open Eventring

let list_ref = ref []
let total_sizes = ref 0
let total_promoted = ref 0

let alloc domain_id ts sizes =
  let size_accum = Array.fold_left (fun x y -> x + y) 0 sizes in
    total_sizes := !total_sizes + size_accum

let runtime_counter domain_id ts counter value =
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
    let callbacks = Callbacks.create ~runtime_counter ~alloc () in
    ignore(read_poll cursor callbacks None);
    Printf.printf "total_sizes: %d, total_promoted: %d\n" !total_sizes !total_promoted