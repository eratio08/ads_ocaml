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

(* Lazy Queue that has an amortized run time of O(1) for all operations. *)
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