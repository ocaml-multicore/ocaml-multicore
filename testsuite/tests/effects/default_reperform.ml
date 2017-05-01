effect E : int -> int with function E i -> abs i
effect T : char -> unit
effect U : float -> int with function U f -> perform (E (int_of_float f))

let _ =
  match
    begin match
      begin match
        let i = perform (E 42) in
        perform (U (float_of_int i)) (* -42 *)
      with
      | x -> perform (E (x+1))       (* 41 *)
      | effect (U f) k -> continue k (int_of_float ((-.1.0) *. f))
      end
    with
    | x -> perform (U (float_of_int (x-2*x+1))) (* 40 *)
    | effect (T c) k -> continue k ()
    end
  with
  | x -> Printf.printf "%d\n" (perform (E (x-1))) (* 39 *)
