(* TEST
 *)

type _ eff += Foo : int -> int eff

let r =
  let h' : type a. a eff -> (a, 'b) continuation -> 'b = function
    | Foo i -> (fun k -> failwith "NO")
    | e -> (fun k -> reperform e k)
  in
  let h : type a. a eff -> (a, 'b) continuation -> 'b = function
    | Foo i -> (fun k ->
        match_with (fun () -> continue k (i+1))
        { retc = (fun v -> v);
          exnc = (fun e -> raise e);
          effc = h' })
    | e -> (fun k -> reperform e k)
  in
  match_with (fun () -> perform (Foo 3))
  { retc = (fun v -> v);
    exnc = (fun e -> raise e);
    effc = h }

let () = Printf.printf "%d\n" r
