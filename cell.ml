open Core

type t =
  { row    : int
  ; column : int
  ; value  : int option
  }
  [@@deriving fields]

let all_cells =
  List.fold (List.range 0 9) ~init:[] ~f:(fun l row ->
      List.fold (List.range 0 9) ~init:l ~f:(fun l' column ->
          { row ; column ; value = None } :: l'
        )
    )
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
