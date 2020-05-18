(** Lightweight threads. *)

type t

val id: t -> int
val self: unit -> t
val create: ('a -> 'b) -> 'a -> t
val join: t -> unit
val delay: float -> unit
val yield: unit -> unit
val exit: unit -> unit
val kill: t -> unit

val select:
  Unix.file_descr list -> Unix.file_descr list -> Unix.file_descr list ->
  float -> Unix.file_descr list * Unix.file_descr list * Unix.file_descr list

val wait_read: Unix.file_descr -> unit
val wait_write: Unix.file_descr -> unit
val wait_timed_read: Unix.file_descr -> float -> bool
val wait_timed_write: Unix.file_descr -> float -> bool
val wait_pid: int -> int * Unix.process_status
val wait_signal: int list -> int

module Internal: sig
  type exn += private Not_joinable

  val incomplete: m:string -> string -> 'a

  module Verbose: sig
    open Domain

    val enable: unit -> unit
    val log: m:string -> string -> char -> unit

    val notify: id -> unit
    val wait: unit -> unit
    val wait_until: nanoseconds -> Sync.timeout_or_notified
  end

  type quantum = Full | Empty

  type spin

  module Spin: sig
    val create: quantum -> spin
    val state: spin -> quantum
    val obtain: spin -> bool
    val release: spin -> unit
  end

  type flag = Ready | Wait

  type gate

  module Gate: sig
    val create: flag -> gate
    val enter: gate -> Domain.id -> flag
    val leave: gate -> Domain.id
    val is_ready: gate -> Domain.id -> bool
  end

  module Interlock: sig
    val obtain: unit -> bool
    val release: unit -> unit
    val enter: Domain.id -> flag
    val leave: unit -> Domain.id
    val is_current: Domain.id -> bool
    val yield : Domain.id -> Domain.id
  end

  type thread

  module Catalog: sig
    val obtain: unit -> bool
    val release: unit -> unit
    val require: Domain.id -> thread
    val insert: unit Domain.t -> thread
    val remove: thread -> unit
  end
end
