module type Sortable = functor (Elem : Ord.Ord) -> sig
  type e = Elem.t
  type t

  val empty : t
  val add : e -> t -> t
  val sort : t -> e list
end

module LazySortableList (Elem : Ord.Ord) : sig
  type e = Elem.t
  type t = int * e list list Stdlib.Lazy.t

  include Sortable : functor(Elem) -> with type e := e and type t := t
end = struct
  type e = Elem.t

  (** A lazy sortable list has a size and a monolithic suspension list containing segments which are list of elements.
      Segments are stored in by ascending order of size.
      Elements in a segment are store in ascending order. *)
  type t = int * e list list Stdlib.Lazy.t

  let empty = 0, lazy []

  (** Merges two lists.
      If one list is empty return the non empty list.
      If both list are not empty compare the heads of each list.
      Prepped the smaller head to the merge result of the tail of the list and the other list. *)
  let rec mrg l1 l2 =
    match l1, l2 with
    | xs, [] -> xs
    | [], ys -> ys
    | x :: xs, y :: _ when x <= y -> x :: mrg xs l2
    (* x > y *)
    | l1, y :: ys -> y :: mrg l1 ys
  ;;

  (** Adds an segment to the list.
      Increase the size by one. If the existing segments are even prepped the new element.
      If the existing segments are uneven, merge the new segment with the head segment of the existing segments.
      Then recursively add this new segment with the fail of the existing segments and divide the size by two. *)
  let add x (size, segs) =
    let rec add_seg seg segs size =
      match size mod 2 with
      | 0 -> seg :: segs
      | _ -> add_seg (mrg seg (List.hd segs)) (List.tl segs) (size / 2)
    in
    size + 1, lazy (add_seg x (Stdlib.Lazy.force segs) size)
  ;;

  let sort (_, segs) =
    let rec mrg_all xs segs =
      match xs, segs with
      | xs, [] -> xs
      | xs, seg :: segs -> mrg_all (mrg xs seg) segs
    in
    mrg_all [] (Stdlib.Lazy.force segs)
  ;;
end


