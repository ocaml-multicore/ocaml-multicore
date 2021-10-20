(* TEST
* hasunix
include unix
** bytecode
** native
*)

let check_dls () =
  let k1 = ThreadLocal.new_key (fun () -> 10) in
  let k2 = ThreadLocal.new_key (fun () -> 1.0) in
  ThreadLocal.set k1 100;
  ThreadLocal.set k2 200.0;
  let v1 = ThreadLocal.get k1 in
  let v2 = ThreadLocal.get k2 in
  assert (v1 = 100);
  assert (v2 = 200.0);
  Gc.major ()

let check_dls_domain_reuse () =
  let k1 = ThreadLocal.new_key (fun () -> 100) in
  let k2 = ThreadLocal.new_key (fun () -> 200) in
  let domains = Array.init 4 (fun _ -> Domain.spawn(fun _ ->
    ThreadLocal.set k1 31415;
    ThreadLocal.set k2 27182;
    assert (ThreadLocal.get k1 = 31415);
    assert (ThreadLocal.get k2 = 27182))) in
  Array.iter Domain.join domains;
  Gc.full_major ();
  let domains2 = Array.init 4 (fun _ -> Domain.spawn(fun _ ->
    assert(ThreadLocal.get k1 = 100);
    assert(ThreadLocal.get k2 = 200))) in
  Array.iter Domain.join domains2

let _ =
  let domains = Array.init 3 (fun _ -> Domain.spawn(check_dls)) in
  check_dls ();
  Array.iter Domain.join domains;
  check_dls_domain_reuse ();
  print_endline "ok"
