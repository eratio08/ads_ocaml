module type Cell = sig
  type 'a t [@@deriving show]

  val make : 'a -> 'a t
  val get : 'a t -> 'a
  val set : 'a t -> 'a -> unit
end

module ArrayCell : Cell = struct
  type 'a t = 'a array [@@deriving show]

  let make x = Array.make 1 x
  let get t = t.(0)
  let set t x = t.(0) <- x
end

module NativeCell : Cell = struct
  type 'a t = 'a ref [@@deriving show]

  let make x = ref x
  let get t = !t
  let set t x = t := x
end

(* desugared native cell *)
module StructCell : Cell = struct
  type 'a t = { mutable contents : 'a } [@@deriving show]

  let make x = { contents = x }
  let get t = t.contents
  let set t x = t.contents <- x
end
