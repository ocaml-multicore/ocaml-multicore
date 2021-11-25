(* TEST *)
let () = Random.init 42

let f () =
  let a = Random.int 100 in
  let b = Random.int 100 in
  let c = Random.int 100 in
  (a, b, c)

(* We intentionally spawn many more domains than hardware threads, to
   ensure that we see computations scheduled in a a non-deterministic
   order. With a small number of domains, it's hard to reliably
   observe non-determinism. *)
let domain_count = 1000
let domains = List.init domain_count (fun _i -> Domain.spawn f)
let results = List.map Domain.join domains
let results = List.filteri (fun i _ -> i mod (domain_count / 10) = 0) results

let () =
  results |> List.iter (fun (a, b, c) ->
    Printf.printf "%d %d %d\n%!" a b c
  )

let () =
  print_endline
    "Note: we observe in this output that the random numbers of each child domain\n\
     appear uncorrelated, yet are produced deterministically."
