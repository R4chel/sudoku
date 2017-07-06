open Core

module T = struct 
  type t =
    { row    : int
    ; column : int
    }
    [@@deriving hash, sexp, fields, compare]
  end
  let compare = compare
  let hash = Hashtbl.hash
include T
include Hashable.Make (T)
include Comparable.Make(T)

let all =
  List.fold (List.range 0 9) ~init:Set.empty ~f:(fun s row ->
      List.fold (List.range 0 9) ~init:s ~f:(fun s' column ->
          Set.add s' { row ; column }
        )
    )
;;

let column_compare t1 t2 =
  Int.compare t1.column t2.column
;;

let square t =
  match t.row, t.column with
  | ( 0 | 1 | 2 ), ( 0 | 1 | 2 ) -> 0
  | ( 0 | 1 | 2 ), ( 3 | 4 | 5 ) -> 1
  | ( 0 | 1 | 2 ), ( 6 | 7 | 8 ) -> 2
  | ( 3 | 4 | 5 ), ( 0 | 1 | 2 ) -> 3
  | ( 3 | 4 | 5 ), ( 3 | 4 | 5 ) -> 4
  | ( 3 | 4 | 5 ), ( 6 | 7 | 8 ) -> 5
  | ( 6 | 7 | 8 ), ( 0 | 1 | 2 ) -> 6
  | ( 6 | 7 | 8 ), ( 3 | 4 | 5 ) -> 7
  | ( 6 | 7 | 8 ), ( 6 | 7 | 8 ) -> 8
  | _ -> failwith "invalid row or column"
;;
 
let of_row_column_ids ~row_id ~column_id =
  match row_id, column_id with
  | Id.Row row, Id.Column column -> { row ; column }
  | _ -> failwith "Invalid arguments"
;;
