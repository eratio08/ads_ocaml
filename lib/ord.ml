module type Compare = sig
  type t

  val compare : t -> t -> int
end

module type Ord = sig
  include Compare

  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
end

module type OrdShow = sig
  include Ord

  val pp : Format.formatter -> t -> unit
end

module Ord (T : Compare) = struct
  include T

  let ( < ) t1 t2 = compare t1 t2 < 0
  let ( <= ) t1 t2 = compare t1 t2 <= 0
  let ( > ) t1 t2 = compare t1 t2 > 0
  let ( >= ) t1 t2 = compare t1 t2 >= 0
  let min t1 t2 = if t1 < t2 then t1 else t2
  let max t1 t2 = if t1 > t2 then t1 else t2
end
