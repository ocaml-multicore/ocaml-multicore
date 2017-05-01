let cas_int r v1 v2 =
  Obj.compare_and_swap_field (Obj.repr r) 0 (Obj.repr v1) (Obj.repr v2)

let wait = ref 2

let x = ref false
let y = ref false

type regs = {
  mutable r1 : bool;
  mutable r2 : bool
}

let regs = {r1 = false; r2 = false}

let rec domain1 n =
  if n = 0 then ()
  else begin
    y := true;
    regs.r1 <- !x;
    (* wait for domain2 to finish *)
    while (not (!wait = 1)) do () done;
    (* indicate domain1 finish *)
    assert (cas_int wait 1 0);
    (* wait for main domain to finish *)
    while (!wait = 0) do () done;
    (* dummy cas for barrier *)
    assert (not (cas_int wait 0 0));
    domain1 (n-1)
  end

let domain2_ref = ref 0

let rec domain2 n =
  if n = 0 then ()
  else begin
    x := true;
    regs.r2 <- !y;
    (* indicate domain2 finish *)
    assert (cas_int wait 2 1);
    (* wait for domain1 to finish *)
    while (!wait = 1) do () done;
    (* wait for main domain to finish *)
    while (!wait = 0) do () done;
    (* dummy cas for barrier *)
    assert (not (cas_int wait 0 0));
    domain2 (n-1)
  end

type result = {
  mutable tt : int;
  mutable tf : int;
  mutable ft : int;
  mutable ff : int
}

let main () =
  let r = {tt = 0; tf = 0; ft = 0; ff = 0} in
  let rec loop n =
    if n = 0 then ()
    else begin
      while (not (!wait = 0)) do () done;
      assert (cas_int wait 0 0);
      begin match regs.r1, regs.r2 with
      | true, true   -> r.tt <- r.tt + 1
      | false, true  -> r.ft <- r.ft + 1
      | true, false  -> r.tf <- r.tf + 1
      | false, false -> r.ff <- r.ff + 1
      end;
      x := false;
      y := false;
      assert (cas_int wait 0 2);
      loop (n-1)
    end
  in

  let num_iters =
    if Array.length Sys.argv = 1 then 100000
    else int_of_string (Sys.argv.(1))
  in
  Domain.spawn (fun () -> domain1 num_iters);
  Domain.spawn (fun () -> domain2 num_iters);
  loop num_iters;
  Printf.printf "tt=%d tf=%d ft=%d ff=%d\n" r.tt r.tf r.ft r.ff
(*   if r.tt = 0 then print_endline "ok" else print_endline "fail" *)

let _ = main ()
