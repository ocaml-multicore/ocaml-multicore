module M = struct
  effect E : int -> unit
    with function
    | E i -> Printf.printf "%d\n" i
end

effect T = M.E

let _ =
  perform (T 100)
