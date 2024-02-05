module type Queue = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool

  (* cons backwards to cons, append on the right *)
  val snoc : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a option
  val head_exn : 'a t -> 'a
  val tail : 'a t -> 'a t option
  val tail_exn : 'a t -> 'a t
end

module FifoQueue : Queue = struct
  type 'a t = 'a list * 'a list

  let empty = [], []

  let is_empty = function
    | [], _ -> true
    | _ -> false
  ;;

  (* invariant front can only be empty if rear is also empty *)
  let checkf = function
    | [], r -> List.rev r, []
    | q -> q
  ;;

  let head = function
    | [], _ -> None
    | x :: _, _ -> Some x
  ;;

  let head_exn t = head t |> Option.get

  let tail = function
    | [], _ -> None
    | _ :: f, r -> Some (checkf (f, r))
  ;;

  let tail_exn t = tail t |> Option.get
  let snoc x (f, r) = checkf (f, x :: r)
end

module type Dequeue = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool

  (* insert, inspect, and remove the front element *)
  val cons : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a option
  val head_exn : 'a t -> 'a
  val tail : 'a t -> 'a t option
  val tail_exn : 'a t -> 'a t

  (* insert, inspect, and remove rear element *)
  val snoc : 'a -> 'a t -> 'a t
  val last : 'a t -> 'a option
  val last_exn : 'a t -> 'a
  val init : 'a t -> 'a t option
  val init_exn : 'a t -> 'a t
end

module FiFoDequeue = struct
  type 'a t = 'a list * 'a list

  let empty = [], []

  let is_empty = function
    | [], _ -> true
    | _ -> false
  ;;

  (* if element count is >= 2 font and rear must not be empty *)
  let check = function
    | [], r :: [] -> [ r ], []
    | [], x :: r -> List.rev r, [ x ]
    | x :: f, [] -> [ x ], List.rev f
    | q -> q
  ;;

  let cons x = function
    | f, r -> check (x :: f, r)
  ;;

  let head = function
    | [], _ -> None
    | x :: _, _ -> Some x
  ;;

  let head_exn t = head t |> Option.get

  let tail = function
    | [], _ -> None
    | _ :: f, r -> Some (check (f, r))
  ;;

  let tail_exn t = tail t |> Option.get
  let snoc x (f, r) = check (f, x :: r)

  let last = function
    | [], _ -> None
    | x :: _, [] -> Some x
    | _, x :: _ -> Some x
  ;;

  let last_exn t = last t |> Option.get

  let init = function
    | [], _ -> None
    | f, _ :: r -> Some (check (f, r))
    | q -> Some (check q)
  ;;

  let init_exn t = init t |> Option.get
end

(* Lazy Queue that has an amortized run time of O(1) for all operations.
   Using incremental suspension, by utilizing streams. *)
module StreamQueue = struct
  open Stream

  type 'a t = int * 'a Stream.t * int * 'a Stream.t

  let empty = 0, Stream.Nil, 0, Stream.Nil

  let is_empty = function
    | 0, Stream.Nil, _, _ -> true
    | _ -> false
  ;;

  (* Reverse r whenever r > f *)
  let check = function
    | (len_f, _, len_r, _) as q when len_r <= len_f -> q
    | len_f, f, len_r, r ->
      len_f + len_r, Stream.( ++ ) f (Stream.reverse r), 0, Stream.Nil
  ;;

  let snoc x (len_f, f, len_r, r) = check (len_f, f, len_r + 1, Stream.Cons (lazy (x, r)))

  let head = function
    | _, Stream.Nil, _, _ -> None
    | _, Stream.Cons (lazy (x, _)), _, _ -> Some x
  ;;

  let head_exn t = head t |> Option.get

  let tail = function
    | _, Stream.Nil, _, _ -> None
    | len_f, Stream.Cons (lazy (_, f)), len_r, r -> Some (check (len_f - 1, f, len_r, r))
  ;;

  let tail_exn t = tail t |> Option.get
end

(** Lazy Queue, using monolithic suspension. *)
module LazyQueue = struct
  (** The lazy queue consists of:
      1) a working copy of the suspended front list
      2) the size of the front list
      3) the suspended front of the list
      4) the size of the rear list
      5) the rear list *)
  type 'a t = 'a list * int * 'a list Stdlib.Lazy.t * int * 'a list

  (** Check if the working copy needs to be set.
      If the working copy is empty evaluate the front list and make it the working copy. *)
  let check_w = function
    (* use force here to not evaluate while matching the pattern *)
    | [], len_f, f, len_r, r -> Stdlib.Lazy.force f, len_f, f, len_r, r
    | q -> q
  ;;

  (** Check if the invariant that front list is always at least as long as the rear list.
      If the rear list is shorter or equal to the front list, check if the working copy need to be created.
      If the rear list is longer, lazy reverse the rear list and append it to the front list,
      the rear list is empty now. *)
  let check = function
    | (_, len_f, _, len_r, _) as q when len_r <= len_f -> check_w q
    (* len_r > lenf_f *)
    | _, len_f, (lazy f), len_r, r ->
      check_w (f, len_f + len_r, lazy (f @ List.rev r), 0, [])
  ;;

  (** Append to the rear. *)
  let snoc x (w, len_f, f, len_r, r) = check (w, len_f, f, len_r + 1, x :: r)

  (** Return the head of the queue.
      Using the head of the working copy prefix. *)
  let head = function
    | [], _, _, _, _ -> None
    | x :: _, _, _, _, _ -> Some x
  ;;

  let head_exn t = head t |> Option.get

  (** Return the tail of the queue.
      Removes the head of the working copy and lazily tails the front queue and reduces the tracked length by one. *)
  let tail = function
    | [], _, _, _, _ -> None
    | _ :: w, len_f, (lazy f), len_r, r ->
      Some (check (w, len_f - 1, lazy (List.tl f), len_r, r))
  ;;

  let tail_exn t = tail t |> Option.get
end
