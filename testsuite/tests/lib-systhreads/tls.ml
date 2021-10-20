(* TEST
* hassysthreads
include systhreads
** bytecode
** native
*)

let check_tls () =
  let k1 = ThreadLocal.new_key (fun () -> 10) in
  let k2 = ThreadLocal.new_key (fun () -> 1.0) in
  ThreadLocal.set k1 100;
  ThreadLocal.set k2 200.0;
  let v1 = ThreadLocal.get k1 in
  let v2 = ThreadLocal.get k2 in
  assert (v1 = 100);
  assert (v2 = 200.0);
  Gc.major ()

let _ =
  let threads = Array.init 3 (fun _ -> Thread.create check_tls ()) in
  check_tls ();
  Array.iter Thread.join threads;
  print_endline "ok"
