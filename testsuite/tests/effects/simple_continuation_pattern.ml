effect E : string
let run f =
  match f () with
  | s ->
     print_endline s
  | effect E k ->
     continue k "hello"

let some_f () =
  perform E
let () =
  run some_f
