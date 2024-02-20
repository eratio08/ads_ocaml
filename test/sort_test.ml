open Ads_ocaml.Sort

module IntLazySortableList = struct
  module OrdInt = Ads_ocaml.Ord.Ord (Int)
  include LazySortableList (OrdInt)

  let pp fmt (t : t) =
    match t with
    | s, (lazy []) -> Format.fprintf fmt "(%d, lazy [])" s
    | s, (lazy l) ->
      Format.fprintf
        fmt
        "(%d, lazy %a)"
        s
        Fmt.(brackets (list ~sep:semi (brackets (list ~sep:semi int))))
        l
  ;;

  let equal (t1 : t) (t2 : t) : bool =
    match t1, t2 with
    | (n1, (lazy [])), (n2, (lazy [])) when n1 = n2 -> true
    | (n1, (lazy l1)), (n2, (lazy l2))
      when n1 = n2 && List.equal (List.equal Int.equal) l1 l2 -> true
    | _ -> false
  ;;
end

let int_lazy_sortable_list =
  Alcotest.testable IntLazySortableList.pp IntLazySortableList.equal
;;

(* add *)
let test_add_lazy_list () =
  let open IntLazySortableList in
  Alcotest.(check int_lazy_sortable_list) "add empty" (1, lazy [ [ 1 ] ]) (add 1 empty);
  Alcotest.(check int_lazy_sortable_list)
    "add non-empty"
    (2, lazy [ [ 1; 2 ] ])
    (add 1 empty |> add 2);
  Alcotest.(check int_lazy_sortable_list)
    "add non-empty reversed"
    (3, lazy [ [ 1 ]; [ 2; 3 ] ])
    (add 3 empty |> add 2 |> add 1)
;;

(* sort *)

let suite = [ "LazySortableList.add", `Quick, test_add_lazy_list ]
