(* TEST *)
let () = Random.init 42

let f () =
  let a = Random.int 100 in
  let b = Random.int 100 in
  let c = Random.int 100 in
  (a, b, c)

let domains = List.init 10 (fun _ -> Domain.spawn f)
let results = List.map Domain.join domains

let () =
  results |> List.iter (fun (a, b, c) ->
    Printf.printf "%d %d %d\n%!" a b c
  )

let () =
  print_endline
    "Note: we observe in this output that the random numbers of each child domain\n\
     appear uncorrelated, yet are produced deterministically."
