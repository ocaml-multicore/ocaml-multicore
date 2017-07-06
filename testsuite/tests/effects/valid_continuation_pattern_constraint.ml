type 'a r = {f: 'a}
type 'a b = Var of 'a r
let val_of_b (Var {f}) = f

effect E : 'a b
let run f =
  match f () with
  | s ->
     print_endline s
  | effect E (k : (int b, unit) continuation) ->
     continue k (Var {f=42})

let some_f () =
  string_of_int @@ val_of_b @@ perform E
let () =
  run some_f
