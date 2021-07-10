(* TEST
 *)

type _ eff += E : unit eff

let h : type a. a eff -> (a,'b) continuation -> 'b = function
  | E -> fun k -> 11
  | e -> fun k -> reperform e k

let () =
  Printf.printf "%d\n%!" @@
    match_with (fun _ -> 10)
    { retc = (fun v -> v);
      exnc = (fun ex -> raise ex);
      effc = h }
