module Raw = struct
  (* Low-level primitives provided by the runtime *)
  type t = private int
  external critical_adjust : int -> unit
    = "caml_ml_domain_critical_section"
  external interrupt : t -> unit
    = "caml_ml_domain_interrupt"
  external wait : unit -> unit
    = "caml_ml_domain_yield"
  type timeout_or_notified = Timeout | Notified
  external wait_until : int64 -> timeout_or_notified
    = "caml_ml_domain_yield_until"
  external spawn : (unit -> unit) -> t
    = "caml_domain_spawn"
  external self : unit -> t
    = "caml_ml_domain_id"
  external cpu_relax : unit -> unit
    = "caml_ml_domain_cpu_relax"
end

type nanoseconds = int64
external timer_ticks : unit -> (int64 [@unboxed]) =
  "caml_ml_domain_ticks" "caml_ml_domain_ticks_unboxed" [@@noalloc]

module Sync = struct
  exception Retry
  let rec critical_section f =
    Raw.critical_adjust (+1);
    match f () with
    | x -> Raw.critical_adjust (-1); x
    | exception Retry -> Raw.critical_adjust (-1); Raw.cpu_relax (); critical_section f
    | exception ex -> Raw.critical_adjust (-1); raise ex

  let notify d = Raw.interrupt d
  let wait () = Raw.wait ()
  type timeout_or_notified =
    Raw.timeout_or_notified =
      Timeout | Notified
  let wait_until t = Raw.wait_until t
  let wait_for dt = Raw.wait_until (Int64.add (timer_ticks ()) dt)
  let cpu_relax () = Raw.cpu_relax ()
end

type id = Raw.t

type 'a state =
| Running
| Joining of ('a, exn) result option ref * id
| Finished of ('a, exn) result
| Joined

type 'a t =
  { domain : Raw.t; state : 'a state Atomic.t }

exception Retry
let rec spin f =
  try f () with Retry ->
     (* fixme: spin more gently *)
     spin f

let cas r vold vnew =
  if not (Atomic.compare_and_set r vold vnew) then raise Retry


exception Spawn_failure of string
let spawn f =
  let state = Atomic.make Running in
  let body () =
    let result = match f () with
      | x -> Ok x
      | exception ex -> Error ex in
    (* Begin a critical section that is ended by domain
       termination *)
    Raw.critical_adjust (+1);
    spin (fun () ->
      match Atomic.get state with
      | Running ->
         cas state Running (Finished result)
      | Joining (r, d) as old ->
         cas state old Joined;
         r := Some result;
         Raw.interrupt d
      | Joined | Finished _ ->
         failwith "internal error: I'm already finished?") in
  let domain =
    try
      Raw.spawn body
    with Failure msg ->
      raise @@ Spawn_failure (": Failure: " ^ msg) in
  { domain; state }

let join { domain ; state } =
  let res = spin (fun () ->
    match Atomic.get state with
    | Running ->
       let res = ref None in
       cas state Running (Joining (res, Raw.self ()));
       spin (fun () ->
         Sync.critical_section (fun () ->
           match !res with
           | None -> Sync.wait (); raise Retry
           | Some r -> r))
    | Finished res as old ->
       cas state old Joined;
       res
    | Joining _ | Joined ->
       raise (Invalid_argument "This domain has already been joined")) in
  (* Wait until the domain has terminated.
     The domain is in a critical section which will be
     ended by the runtime when it terminates *)
  Sync.notify domain;
  match res with
  | Ok x -> x
  | Error ex -> raise ex


let get_id { domain; _ } = domain

let self () = Raw.self ()

