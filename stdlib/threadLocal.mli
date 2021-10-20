type 'a key
(** Type of a storage key *)

val create_tls : (unit -> unit)
  (** [create_tls] initializes the thread-local storage for the calling
      thread. This is normally already done when a thread is started.
      You do not need to call this function. *)

val new_key : (unit -> 'a) -> 'a key
(** [new_key f] returns a new key bound to initialiser [f] for accessing
    domain-local variable. *)

val set : 'a key -> 'a -> unit
(** [set k v] updates the calling domain's domain-local state to associate
    the key [k] with value [v]. It overwrites any previous values associated
    to [k], which cannot be restored later. *)

val get : 'a key -> 'a
(** [get k] returns [v] if a value [v] is associated to the key [k] on
    the calling domain's domain-local state. Sets [k]'s value with its
    initialiser and returns it otherwise. *)
