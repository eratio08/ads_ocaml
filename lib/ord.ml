module type Ord = sig
  type t

  val compare : t -> int
  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
end
