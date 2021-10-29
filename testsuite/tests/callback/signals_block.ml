(* TEST
  modules = "signals_block_.c"
   * hassysthreads
   include systhreads
   ** libunix
   *** bytecode
   *** native
*)

(* in this test we have one thread that spends all of
   its time in C and another that has a signal blocked
   and is supposed to do something in OCaml. This test
   tries to trigger the behaviour in issue ocaml-multicore#703*)

external spend_time_idling_in_c : unit -> unit = "spend_time_idling_in_c"
external increment_thing : unit -> unit = "increment_thing"
external get_thing : unit -> int = "get_thing"

let got_signal_flag = Atomic.make false

let f () =
  spend_time_idling_in_c ()

let () =
  let d = Domain.spawn f in
    (* Wait until we're sure the thread is in C *)
    while get_thing () == 0 do
      ()
    done;
    ignore(Unix.sigprocmask SIG_BLOCK [Sys.sigusr1]);
    ignore(Sys.signal Sys.sigusr1 (Signal_handle(fun _ ->
      Atomic.set got_signal_flag true
    )));
    Unix.kill (Unix.getpid ()) Sys.sigusr1;
    for a = 1 to 999_999 do
      increment_thing ()
    done;
    if Atomic.get got_signal_flag then
      (* If we got this signal now, then something has gone wrong with
         delivery. *)
      assert(false);
    increment_thing (); (* release our thread that's in C, it should
                           trigger the signal handler in the
                           leave_blocking_section hook *)
    let () = Domain.join d in
      assert(Atomic.get got_signal_flag)
