(* TEST
 *)

type _ eff += E : unit eff
exception X

let () =
  let h : type a. a eff -> (a, 'b) continuation -> 'b = function
    | E -> (fun k -> 11)
    | e -> (fun k -> reperform e k)
  in
  Printf.printf "%d\n%!" @@
  match_with (fun () ->
    Printf.printf "in handler. raising X\n%!";
    raise X)
  { retc = (fun v -> v);
    exnc = (function
      | X -> 10
      | e -> raise e);
    effc = h }
