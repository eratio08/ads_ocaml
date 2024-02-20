module type Sortable = sig
  type e
  type t

  val empty : t
  val add : e -> t -> t
  val sort : t -> e list
end

module LazySortableList (Elem : Ord.Ord) : sig
  type e = Elem.t
  type t = int * e list list Stdlib.Lazy.t

  include Sortable with type e := e and type t := t
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
      Increase the size by one and add the segment to the lazy segment list.
      When adding elements to the list the order is maintained.
      Amortized complexity is O(log n). *)
  let add (x : e) ((size, segs) : t) : t =
    (* If the existing segments are even prepped the new element.
       If the existing segments are uneven, merge the new segment with the head segment of the existing segments.
       Then recursively add this new segment with the tail of the existing segments and divide the size by two. *)
    let rec add_seg seg segs size =
      match size mod 2 with
      | 0 -> seg :: segs
      | _ -> add_seg (mrg seg (List.hd segs)) (List.tl segs) (size / 2)
    in
    size + 1, lazy (add_seg [ x ] (Stdlib.Lazy.force segs) size)
  ;;

  (** Sorts the given lazy list my merging it with an empty list.
      Merging with an empty list will cause a merging cascade though all segments there by sorting the elements.
      Amortized complexity is O(n). *)
  let sort ((_, segs) : t) : e list =
    let rec mrg_all xs segs =
      match xs, segs with
      | xs, [] -> xs
      | xs, seg :: segs -> mrg_all (mrg xs seg) segs
    in
    mrg_all [] (Stdlib.Lazy.force segs)
  ;;
end
