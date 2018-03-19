effect E : unit

let _ = 
   match perform E with
   | _ -> failwith "Unhandled wasn't caught."
   | exception Unhandled -> print_endline "()"
