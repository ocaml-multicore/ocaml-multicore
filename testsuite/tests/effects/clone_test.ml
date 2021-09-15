(* TEST
 *)

open Obj.Effect_handlers
open Obj.Effect_handlers.Deep

module Segfault = struct
  module Nondet = struct
    type _ eff += Fork : bool eff
    let fork () = perform Fork

    let rec forkEach f = function
      | [] -> ()
      | x::xs ->
         (* if branches are swapped, then no segfault occurs *)
         if (fork ()) then (f x) else (forkEach f xs)

    (** Explores alternatives in BFS *)
    let run worlds action =
      let scheduleNext () =
        begin match !worlds with
        | [] -> ()
        | w::ws -> worlds := ws; w ()
        end
      in
      match_with action
      { retc = (fun x -> scheduleNext ());
        exnc = (fun e -> raise e);
        effc = fun (type a) (e : a eff) ->
          match e with
          | Fork -> Some (fun (k : (a, 'b) continuation) ->
              let k2 = clone_continuation k in
              let choices = [(fun () -> continue k true); (fun () -> continue k2 false)] in
              worlds := !worlds @ choices;
              scheduleNext ())
          | e -> None }

    let handle action = run (ref []) action
  end

  let randArray n =
    Array.init n (fun i -> Random.int 1073741823)

  type _ eff += Yield: int -> unit eff
  let yield v = perform (Yield v)

  let boom () =
    let unyield action () =
      try_with action
      { effc = fun (type a) (e : a eff) ->
          match e with
          | Yield _ -> Some (fun (k : (a, _) continuation) ->
              try discontinue k Exit with Exit -> ())
          | e -> None }
    in
    let state = ref [] in
    let stateful action () =
      try_with action
      { effc = fun (type a) (e : a eff) ->
          match e with
          | Yield i -> Some (fun (k : (a, _) continuation) ->
              state := i :: !state;
              (* Important: each yield invocation does not return. *)
              Nondet.forkEach yield !state;
              (* Exactly one of the forked computations will return here. *)
              continue k ())
          | e -> None }
    in
    let count = 10000 in (* for count < 10000, no segfault *)
    let a1 = randArray count in
    let iter () = Array.iter yield a1 in
    Nondet.handle (unyield (stateful iter))

  let _ = boom ()
  let _ = print_string "ok"
end
