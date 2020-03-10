module Internal = struct
  open Thread.Internal

  let log = Verbose.log ~m:__MODULE__

  type condition = Condition of {
    spin: spin;
    waiting: Domain.id Queue.t;
  }

  let create () =
    let spin = Spin.create Full and waiting = Queue.create () in
    Condition { spin; waiting }

  let obtain (Condition c) =
    Spin.obtain c.spin && begin
      let result = Interlock.obtain () in
      if not result then Spin.release c.spin;
      result
    end

  let release (Condition c) =
    Interlock.release ();
    Spin.release c.spin
end

open Thread.Internal
open Internal

type t = condition

let create = create

let wait (Condition c) m =
  let log = log "wait" in
  log '>';
  Mutex.Internal.wait m c.spin c.waiting;
  log '<'

let signal (Condition c as c0) =
  let log = log "signal" in
  log '>';
  let aux () =
    if not (obtain c0) then raise Domain.Sync.Retry;
    let self = Domain.self () in
    let ready =
      if Queue.is_empty c.waiting then begin
        log 'E';
        self
      end
      else begin
        log 'Y';
        Queue.pop c.waiting
      end
    in
    release c0;
    if self != ready then Verbose.notify ready
  in
  Domain.Sync.critical_section aux;
  log '<'

let broadcast (Condition c as c0) =
  let log = log "broadcast" in
  log '>';
  let aux () =
    if not (obtain c0) then raise Domain.Sync.Retry;
    let ready = Queue.create () in
    Queue.transfer c.waiting ready;
    release c0;
    Queue.iter Verbose.notify ready
  in
  Domain.Sync.critical_section aux;
  log '<'
