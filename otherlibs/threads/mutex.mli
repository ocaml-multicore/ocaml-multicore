type t

val create: unit -> t
val lock: t -> unit
val unlock: t -> unit
val try_lock: t -> bool

module Internal: sig
  open Thread.Internal
  val wait: t -> spin -> Domain.id Queue.t -> unit
end
