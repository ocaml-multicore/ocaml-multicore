(* TEST *)
open Ephemeron

let kn = Kn.create 10000
let w = Weak.create 10000

let set_values () =
  for i = 0 to 9999 do
    Kn.set_key kn i i;
    Weak.set w i (Some i)
  done;
  Kn.set_data kn 1000

let l = Lazy.from_fun set_values

let rec safe_force l =
  match Lazy.try_force l with
  | Some x -> x
  | None -> (
      Domain.Sync.cpu_relax () ;
      safe_force l
    )

let _ =
  for _ = 0 to 5 do
  let domains = Array.init 4 (fun _ -> Domain.spawn (fun () -> safe_force l)) in
  Array.iter Domain.join domains;
  Gc.full_major ()
  done;
  for i = 0 to 9999 do
    match (Kn.get_key kn i) with
    | Some x -> ()
    | None -> Printf.printf "ephe fail";
    match Weak.get w i with
    | Some x -> ()
    | None -> Printf.printf "weak fail"
  done;
  print_endline "ok"
