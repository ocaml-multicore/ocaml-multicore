(* TEST *)
open Eventring

let major = ref 0
let minor = ref 0
let compact = ref 0
let majors = ref 0
let minors = ref 0
let compacts = ref 0

let got_start = ref false

let lifecycle ts lifecycle_event data =
    match lifecycle_event with
    | EV_RING_START ->
        begin
            assert(match data with
            | Some(pid) -> true
            | None -> false);
            got_start := true
        end
    | _ -> ()

let runtime_begin ts phase =
    match phase with
    | EV_MAJOR_SLICE ->
        begin
            assert(!major == 0);
            major := 1
        end
    | EV_MINOR ->
        begin
            assert(!minor == 0);
            minor := 1
        end
    | _ -> ()

let runtime_end ts phase =
    match phase with
    | EV_MAJOR_SLICE ->
        begin
            assert(!major == 1);
            major := 0;
            incr majors
        end
    | EV_MINOR ->
        begin
            assert(!minor == 1);
            minor := 0;
            incr minors
        end
    | _ -> ()

let lost_events num =
    Printf.printf "Lost %d events\n" num

let epochs = 100

let () =
    let list_ref = ref [] in
    start ();
    let cursor = create_cursor None in
    let callbacks = { 
        ev_runtime_begin = Some(runtime_begin);
        ev_runtime_end = Some(runtime_end);
        ev_runtime_counter = None;
        ev_alloc = None;
        ev_lifecycle = Some(lifecycle);
        ev_lost_events = Some(lost_events)
    } in
    for epoch = 0 to epochs do
        for a = 0 to 100 do
            list_ref := [];
            for a = 0 to 10 do
                list_ref := (Sys.opaque_identity(ref 42)) :: !list_ref
            done;
            Gc.full_major ()
        done;
        ignore(read_poll cursor callbacks None)
    done;
    assert(!got_start);
    Printf.printf "minors: %d, majors: %d\n" !minors !majors