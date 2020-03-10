module Internal = struct
  open Thread.Internal

  let log = Verbose.log ~m:__MODULE__

  type mutex = Mutex of {
    spin: spin;
    gate: gate;
  }

  let create () =
    let spin = Spin.create Full and gate = Gate.create Ready in
    Mutex { spin; gate }

  let obtain (Mutex m) =
    Spin.obtain m.spin && begin
      let result = Interlock.obtain () in
      if not result then Spin.release m.spin;
      result
    end

  let release (Mutex m) =
    Interlock.release ();
    Spin.release m.spin

  module Aux = struct
    let lock (Mutex m as m0) =
      let self = Domain.self () in
      match Gate.enter m.gate self with
      | Ready ->
          release m0;
          true
      | Wait ->
          let ready = Interlock.leave () in
          release m0;
          if ready != self then Verbose.notify ready;
          Verbose.wait ();
          true

    let unlock (Mutex m as m0) =
      let self = Domain.self () and next = Gate.leave m.gate in
      let notify = (next != self) in
      let wait = if notify then Interlock.enter next else Ready in
      release m0;
      if wait == Ready && notify then Verbose.notify next
  end

  let wait (Mutex m as m0) cs cq =
    let log = log "wait" in
    let enter () =
      if not (obtain m0) then raise Domain.Sync.Retry;
      if not (Spin.obtain cs) then begin
        release m0;
        raise Domain.Sync.Retry
      end;
      let self = Domain.self () in
      Queue.add self cq;
      let next = Gate.leave m.gate in
      log (if next != self then 'L' else 'a');
      let ready =
        if next != self then begin
            match Interlock.enter next with
            | Ready -> next
            | Wait -> Interlock.leave ()
        end
        else
            Interlock.leave ()
      in
      Spin.release cs;
      release m0;
      log (if ready != self then 'N' else 'b');
      if ready != self then Verbose.notify ready;
      Verbose.wait ()
    and leave () =
      if not (obtain m0) then raise Domain.Sync.Retry;
      let self = Domain.self () in
      let wait = Gate.enter m.gate self in
      log (if wait == Ready then 'E' else 'x');
      let wait = if wait == Ready then Interlock.enter self else Wait in
      release m0;
      log (if wait != Ready then 'W' else 'y');
      if wait != Ready then Verbose.wait ()
    in
    log '>';
    Domain.Sync.critical_section enter;
    log '|';
    Domain.Sync.critical_section leave;
    log '<'
end

open Internal

type t = mutex

let create = create

let lock m =
  let log = log "lock" in
  log '>';
  let aux () =
    if not (obtain m) then raise Domain.Sync.Retry;
    ignore (Aux.lock m)
  in
  Domain.Sync.critical_section aux;
  log '<'

let try_lock m =
  let log = log "try_lock" in
  log '>';
  let aux () = obtain m && Aux.lock m in
  let ret = Domain.Sync.critical_section aux in
  log (if ret then '<' else '!');
  ret

let unlock m =
  let log = log "unlock" in
  log '>';
  let aux () =
    if not (obtain m) then raise Domain.Sync.Retry;
    Aux.unlock m
  in
  Domain.Sync.critical_section aux;
  log '<'
