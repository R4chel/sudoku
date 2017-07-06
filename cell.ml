open Core

module T = struct 
  type t =
    { row    : int
    ; column : int
    }
    [@@deriving hash, sexp, fields, compare]
  let compare = compare
  let hash = Hashtbl.hash
end
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

let all_cells_by_square square =
  let rows, columns =
    match square with
    | 0 -> [ 0 ; 1 ; 2 ], [ 0 ; 1 ; 2 ]
    | 1 -> [ 0 ; 1 ; 2 ], [ 3 ; 4 ; 5 ]
    | 2 -> [ 0 ; 1 ; 2 ], [ 6 ; 7 ; 8 ]
    | 3 -> [ 3 ; 4 ; 5 ], [ 0 ; 1 ; 2 ]
    | 4 -> [ 3 ; 4 ; 5 ], [ 3 ; 4 ; 5 ]
    | 5 -> [ 3 ; 4 ; 5 ], [ 6 ; 7 ; 8 ]
    | 6 -> [ 6 ; 7 ; 8 ], [ 0 ; 1 ; 2 ]
    | 7 -> [ 6 ; 7 ; 8 ], [ 3 ; 4 ; 5 ]
    | 8 -> [ 6 ; 7 ; 8 ], [ 6 ; 7 ; 8 ]
    | _ -> failwith "Invalid square"
  in
  List.cartesian_product rows columns
  |> List.map ~f:(fun (row, column) -> { row ; column })
;;

let of_row_column_ids ~row_id ~column_id =
  match row_id, column_id with
  | Id.Row row, Id.Column column -> { row ; column }
  | _ -> failwith "Invalid arguments"
;;

let all_by_id (id : Id.t) =
  match id with
  | Row row ->
    List.map Id.columns ~f:(fun column_id ->
        match column_id with
        | Column column -> { row ; column }
        | _ -> failwith "invalid id type"
      )
  | Column column ->
    List.map Id.rows ~f:(fun row_id ->
        match row_id with
        | Row row -> { row ; column }
        | _ -> failwith "invalid id type"
      )
  | Square square -> 
    all_cells_by_square square
;;

let intersecting (t1 : t) (t2 : t) =
  Int.(=) t1.row t2.row
  || Int.(=) t1.column t2.column
  || Int.(=) (square t1) (square t2)
;;

let not_intersecting (t1 : t) (t2 : t) =
  Int.(<>) t1.row t2.row
  && Int.(<>) t1.column t2.column
  && Int.(<>) (square t1) (square t2)
;;

let () =
  let cell1 : t = { row = 2 ; column = 1 }  in
  let cell2 : t = { row = 7 ; column = 7 }  in
  assert (intersecting { row = 2 ; column = 1} { row = 2 ; column = 4} );
  assert (intersecting { row = 2 ; column = 1} { row = 4 ; column = 1} );
  assert (intersecting { row = 2 ; column = 1} { row = 0 ; column = 0} );
  assert (Int.(=) (square cell1) 0);
  assert (Int.(=) (square cell2) 8);
  assert (Int.(<>) (square cell1) (square cell2));
  assert (Int.(<>) (cell1.row) (cell2.row));
  assert (Int.(<>) (cell1.column) (cell2.column));
  assert (not (intersecting cell1 cell2));
  assert (not (intersecting { row = 2 ; column = 1} { row = 7 ; column = 7}));
;;
