(* TEST *)
open Eventring

let major = ref 0
let minor = ref 0
let compact = ref 0
let majors = ref 0
let minors = ref 0
let compacts = ref 0

let got_start = ref false

let lost_events_count = ref 0

let lost_events num_events =
    lost_events_count := !lost_events_count + num_events

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

let num_domains = 5
let num_full_majors = 10

let () =
    start ();
    let cursor = create_cursor None in
    let gc_churn_f () =
        let list_ref = ref [] in
        for j = 0 to num_full_majors do
            list_ref := [];
            for a = 0 to 100 do
                list_ref := (Sys.opaque_identity(ref 42)) :: !list_ref
            done;
            Gc.full_major ();
        done
    in
    let domains_list = List.init num_domains (fun _ -> Domain.spawn gc_churn_f) in
    let _ = List.iter Domain.join domains_list in
    let callbacks = { 
        ev_runtime_begin = Some(runtime_begin);
        ev_runtime_end = Some(runtime_end);
        ev_runtime_counter = None;
        ev_alloc = None;
        ev_lifecycle = Some(lifecycle);
        ev_lost_events = Some(lost_events)
    } in 
    ignore(read_poll cursor callbacks None);
    assert(!got_start);
    (* this is num_full_majors rather than num_full_majors*num_domains
        because in the worst case it can be that low. *)
    assert(!majors >= num_full_majors);
    assert(!lost_events_count == 0)