module Internal = struct
  exception Not_joinable

  let incomplete ~m:mname fname =
    Printf.sprintf "%s.%s: incomplete!" mname fname |> failwith

  module Id_order = struct
    type t = Domain.id

    let compare a b =
      let d = (b : t :> int) - (a : t :> int) in
      if d < 0 then 1 else if d > 0 then (-1) else 0
  end

  module Id_map = Map.Make(Id_order)

  type quantum = Full | Empty
  type spin = Spin of { box: quantum Atomic.t } [@@caml.unboxed]

  module Spin = struct
    let create quantum = Spin { box = Atomic.make quantum }
    let state (Spin s) = Atomic.get s.box
    let obtain (Spin s) = Atomic.compare_and_set s.box Full Empty
    let release (Spin s) = Atomic.set s.box Full
  end

  type dlock = DLock of {
    spin: spin;
    mutable count: int;
    mutable owner: Domain.id;
  }

  module DLock = struct
    let create quantum =
      let spin = Spin.create quantum in
      let count = 0 and owner = Domain.self () in
      DLock { spin; count; owner }

    let obtain (DLock d) =
      if Spin.obtain d.spin then begin
        let self = Domain.self () in
        let ret =
          let count = d.count in
          if count == 0 || d.owner == self then begin
            let count = succ count in
            d.count <- count;
            d.owner <- self;
            count
          end
          else
            0
        in
        Spin.release d.spin;
        ret
      end
      else
        0

    let release (DLock d) =
      while not (Spin.obtain d.spin) do Domain.Sync.cpu_relax () done;
      assert (d.count > 0 && d.owner == Domain.self ());
      d.count <- pred d.count;
      Spin.release d.spin
  end

  module Verbose = struct
    let singleton = DLock.create Full
    let enabled = ref false

    let enable () =
      while DLock.obtain singleton == 0 do Domain.Sync.cpu_relax () done;
      enabled := true;
      DLock.release singleton

    let rec log ~m:mname fname dir =
      match DLock.obtain singleton with
      | 0 ->
          Domain.Sync.cpu_relax ();
          log ~m:mname fname dir
      | n ->
          if n == 1 && !enabled then begin
            let self = Domain.self () in
            Printf.eprintf "%c%d %s.%s\n" dir (self :> int) mname fname;
            flush stderr;
          end;
          DLock.release singleton

    let notify (id : Domain.id) =
      let msg = Printf.sprintf "notify %u" (id :> int) in
      let log = log ~m:"Domain" msg in
      log '>';
      Domain.Sync.notify id;
      log '<'

    let wait () =
      let log = log ~m:"Domain" "wait" in
      log '>';
      Domain.Sync.wait ();
      log '<'

    let wait_until t =
      let msg = Printf.sprintf "wait_until %Lu" t in
      let log = log ~m:"Domain" msg in
      log '>';
      let r = Domain.Sync.wait_until t in
      log begin
        match r with
        | Domain.Sync.Timeout -> '<'
        | Domain.Sync.Notified -> '%'
      end;
      r
  end

  type flag = Ready | Wait

  type gate = Gate of {
    mutable flag: flag;
    mutable ready: Domain.id;
    waiting: Domain.id Queue.t;
  }

  module Gate = struct
    let create flag =
      let ready = Domain.self () in
      let waiting = Queue.create () in
      Gate { flag; ready; waiting }

    let enter (Gate g) id =
       match g.flag with
      | Ready ->
          assert (Queue.is_empty g.waiting);
          g.flag <- Wait;
          g.ready <- id;
          Ready
      | Wait ->
          assert (g.ready != id);
          let check id' = assert (id' != id) in
          Queue.iter check g.waiting;
          Queue.add id g.waiting;
          Wait

    let leave (Gate g) =
      assert (g.flag != Ready);
      if Queue.is_empty g.waiting then begin
        g.flag <- Ready;
        g.ready
      end
      else begin
        let ready = Queue.pop g.waiting in
        g.ready <- ready;
        ready
      end

    let is_ready (Gate g) id = (g.flag == Ready && g.ready == id)
  end

  module Interlock = struct
    let never = false

    type singleton = Singleton of {
      spin: spin;
      mutable gate: gate option;
    }

    let singleton =
      let spin = Spin.create Full in
      let gate = if never then None else Some (Gate.create Wait) in
      Singleton { spin; gate }

    let obtain () =
      if never then true else begin
        let Singleton s = singleton in
        Spin.obtain s.spin
      end

    let release () =
      if not never then begin
        let Singleton s = singleton in
        Spin.release s.spin
      end

    let enter id =
      if never then
        Ready
      else begin
        let Singleton s = singleton in
        match s.gate with
        | None ->
            Ready
        | Some g ->
            Gate.enter g id
      end

    let leave () =
      let self = Domain.self () in
      if never then
        self
      else begin
        let Singleton s = singleton in
        match s.gate with
        | None ->
            self
        | Some g ->
            Gate.leave g
      end

    let is_current id =
      let self = Domain.self () in
      if never then
        self == id
      else begin
        let Singleton s = singleton in
        match s.gate with
        | None -> self == id
        | Some g -> Gate.is_ready g id
      end

    let yield id =
      if never then
        id
      else begin
        let Singleton s = singleton in
        match s.gate with
        | None ->
            id
        | Some g ->
            let _ = Gate.enter g id in
            Gate.leave g
      end
  end

  (* TODO(jhw) --  Thread.t must be reliably hashable. *)
  type thread =
    | Detached of Domain.id
    | Joinable of {
        spin: spin;
        domain: unit Domain.t;
        mutable joiners: Domain.id Queue.t option;
      }

  module Catalog = struct
    type singleton = Singleton of {
      spin: spin;
      mutable threads: thread Id_map.t;
    }

    let singleton =
      let spin = Spin.create Full and threads = Id_map.empty in
      Singleton { spin; threads }

    let obtain () =
      let Singleton s = singleton in
      Spin.obtain s.spin

    let release () =
      let Singleton s = singleton in
      Spin.release s.spin

    let require id =
      let Singleton s = singleton in
      match Id_map.find id s.threads with
      | exception Not_found -> Detached id
      | thread -> thread

    let insert domain =
      let spin = Spin.create Full and joiners = Some (Queue.create ()) in
      let id = Domain.get_id domain in
      let thread = Joinable { spin; domain; joiners } in
      let Singleton s = singleton in
      let threads = s.threads in
      let threads = Id_map.add id thread threads in
      s.threads <- threads;
      thread

    let remove thread =
      match thread with
      | Detached _ ->
          assert (not true)
      | Joinable t ->
          let Singleton s = singleton in
          let id = Domain.get_id t.domain in
          s.threads <- Id_map.remove id s.threads
  end

  external initialize: unit -> unit = "caml_threads_init"

  (* move this into Interlock *)
  let _ =
    let enter_aux () =
      if not (Interlock.obtain ()) then raise Domain.Sync.Retry;
      let ready = Interlock.leave () in
      Interlock.release ();
      let self = Domain.self () in
      if ready != self then Verbose.notify ready
    in
    let leave_aux () =
      if not (Interlock.obtain ()) then raise Domain.Sync.Retry;
      let wait = Domain.self () |> Interlock.enter in
      Interlock.release ();
      if wait != Ready then Verbose.wait ()
    in
    let rec enter () =
      match DLock.obtain Verbose.singleton with
      | 0 ->
          Domain.Sync.cpu_relax ();
          (enter[@tailcall]) ()
      | n ->
          DLock.release Verbose.singleton;
          if n == 1 then begin
            Domain.Sync.critical_section enter_aux;
            Gc.minor ()
          end
     in
    let rec leave () =
      match DLock.obtain Verbose.singleton with
      | 0 ->
          Domain.Sync.cpu_relax ();
          (leave[@tailcall]) ()
      | n ->
          DLock.release Verbose.singleton;
          if n == 1 then Domain.Sync.critical_section leave_aux
    in
    if not Interlock.never then begin
      Callback.register "threads_enter_blocking_section" enter;
      Callback.register "threads_leave_blocking_section" leave
    end;
    initialize ()
end

open Internal

let incomplete = incomplete ~m:__MODULE__
let log = Verbose.log ~m:__MODULE__

type t = Thread of Domain.id [@@caml.unboxed]

let id (Thread n) = (n :> int)

let self =
  let aux () =
    if not (Catalog.obtain ()) then raise Domain.Sync.Retry;
    if not (Interlock.obtain ()) then begin
      Catalog.release ();
      raise Domain.Sync.Retry;
    end;
    let self = Domain.self () in
    match Catalog.require self with
    | Joinable _ ->
        Interlock.release ();
        Catalog.release ();
        Thread self
    | Detached _ ->
        let wait =
          if not (Interlock.is_current self) then
            Interlock.enter self
          else
            Ready
        in
        Interlock.release ();
        Catalog.release ();
        if wait != Ready then Verbose.wait ();
        Thread self
  in
  let enter () = Domain.Sync.critical_section aux in
  enter

let task spin parent procedure initial () =
  let log = log "task" in
  log '>';
  let self = Domain.self () in
  let start () =
    if not (Interlock.obtain ()) then raise Domain.Sync.Retry;
    if not (Spin.obtain spin) then begin
      Interlock.release ();
      raise Domain.Sync.Retry;
    end;
    let wait = Interlock.enter self in
    Interlock.release ();
    Verbose.notify parent;
    if wait != Ready then Verbose.wait ()
  in
  Domain.Sync.critical_section start;
  log '^';
  let finish () =
    if not (Catalog.obtain ()) then raise Domain.Sync.Retry;
    match Catalog.require self with
    | Detached _ ->
        assert (not true);
        Catalog.release ()
    | Joinable t ->
        if not (Spin.obtain t.spin) then begin
          Catalog.release ();
          raise Domain.Sync.Retry
        end;
        if not (Interlock.obtain ()) then begin
          Spin.release t.spin;
          Catalog.release ();
          raise Domain.Sync.Retry
        end;
        let ready = Queue.create () in begin
          match t.joiners with
          | None -> ()
          | Some q -> Queue.transfer q ready
        end;
        t.joiners <- None;
        let other = Interlock.leave () in
        if self != other then Queue.add other ready;
        Interlock.release ();
        Spin.release t.spin;
        Catalog.release ();
        Queue.iter Verbose.notify ready
  in
  match procedure initial with
  | exception Exit ->
      log 'X';
      Domain.Sync.critical_section finish;
      log '<'
  | exception x ->
      log '*';
      let nym = Printexc.to_string x in
      Printf.eprintf "Thread %d killed on uncaught exception %s\n"
        (self :> int) nym;
      flush stderr;
      Domain.Sync.critical_section finish;
      log '<'
  | _ ->
      log 'v';
      Domain.Sync.critical_section finish;
      log '<'

let domain_id_of_thread thread =
  match thread with
  | Detached id -> id
  | Joinable t -> Domain.get_id t.domain

let create procedure initial =
  let log = log "create" in
  log '>';
  let self = Domain.self () in
  let spin = Spin.create Empty in
  let domain = task spin self procedure initial |> Domain.spawn in
  let start () =
    if not (Catalog.obtain ()) then raise Domain.Sync.Retry;
    let thread = Catalog.insert domain in
    Catalog.release ();
    Spin.release spin;
    Verbose.wait ();
    domain_id_of_thread thread
  in
  let id = Domain.Sync.critical_section start in
  log '<';
  Thread id

let join =
  let log = log "join" in
  let phase1 id () =
    if not (Catalog.obtain ()) then raise Domain.Sync.Retry;
    match Catalog.require id with
    | Detached _ ->
        Catalog.release ();
        log '*';
        raise Not_joinable
    | Joinable t ->
        if not (Spin.obtain t.spin) then raise Domain.Sync.Retry;
        match t.joiners with
        | None ->
            Spin.release t.spin;
            Catalog.release ();
            Ready
        | Some q ->
            if not (Interlock.obtain ()) then begin
              Spin.release t.spin;
              Catalog.release ();
              raise Domain.Sync.Retry
            end;
            let self = Domain.self () in
            Queue.add self q;
            let ready = Interlock.leave () in
            Interlock.release ();
            Spin.release t.spin;
            Catalog.release ();
            if ready != self then Verbose.notify ready;
            Verbose.wait ();
            Wait
  and phase2 () =
    if not (Interlock.obtain ()) then raise Domain.Sync.Retry;
    let self = Domain.self () in
    let wait = Interlock.enter self in
    Interlock.release ();
    if wait != Ready then Verbose.wait ()
  in
  let enter (Thread id) =
    log '>';
    let op = Domain.Sync.critical_section (phase1 id) in
    if op == Wait then begin
      log '%';
      Domain.Sync.critical_section phase2
    end;
    log '<'
  in
  enter

let delay interval =
  let now = Domain.timer_ticks () and self = Domain.self () in
  let ns = Int64.of_float (interval *. 1.0e9) in
  let tick = Int64.add now ns in
  let suspend () =
    if not (Interlock.obtain ()) then raise Domain.Sync.Retry;
    let ready = Interlock.leave () in
    Interlock.release ();
    if ready != self then Verbose.notify ready;
    ignore (Verbose.wait_until tick)
  and resume () =
    if not (Interlock.obtain ()) then raise Domain.Sync.Retry;
    let wait = Interlock.enter self in
    Interlock.release ();
    if wait != Ready then Verbose.wait ()
  in
  Domain.Sync.critical_section suspend;
  Domain.Sync.critical_section resume

let yield =
  let log = log "yield" in
  let aux () =
    if not (Interlock.obtain ()) then raise Domain.Sync.Retry;
    let self = Domain.self () in
    let ready = Interlock.leave () in
    let wait = if ready != self then Interlock.enter self else Ready in
    Interlock.release ();
    if wait != Ready then begin
      Verbose.notify ready;
      Verbose.wait ()
    end
  in
  let enter () =
    log '>';
    Domain.Sync.critical_section aux;
    log '<';
  in
  enter

let exit () = raise Exit
let kill _thread = invalid_arg "Thread.kill: not implemented"

let select = Unix.select

(* let wait_read _fd = incomplete "wait_read"
 * let wait_write _fd = incomplete "wait_write"
 * let wait_timed_read _fd _interval = incomplete "wait_timed_read"
 * let wait_timed_write _fd _interval = incomplete "wait_timed_write"
 * let wait_pid _pid = incomplete "wait_pid" *)
let wait_signal _signals = incomplete "wait_signal"

let wait_timed_read fd d =
  match Unix.select [fd] [] [] d with ([], _, _) -> false | (_, _, _) -> true

let wait_timed_write fd d =
  match Unix.select [] [fd] [] d with (_, [], _) -> false | (_, _, _) -> true

let wait_read fd = ignore (Unix.select [fd] [] [] (-1.0))
let wait_write fd = ignore (Unix.select [] [fd] [] (-1.0))

let wait_pid p = Unix.waitpid [] p
