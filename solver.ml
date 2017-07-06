open Core

let possible_values board cell =
  let row_id    = Id.Row    (Cell.row    cell) in
  let column_id = Id.Column (Cell.column cell) in
  let square_id = Id.Square (Cell.square cell) in
  let row    = Board.values_by_id board row_id in
  let column = Board.values_by_id board column_id in
  let square = Board.values_by_id board square_id in
  let all_intersecting_values = row @ column @ square in
  List.fold all_intersecting_values ~init:Value.all ~f:(fun acum num ->
      Value.Set.remove acum num
    )

;;


let possible_cells board empty_cells value =
  (* let empty_cells = Board.empty_cells board in *)
  Map.filter board ~f:(Value.equal value)
  |> Map.keys
  |> List.fold ~init:empty_cells ~f:(fun empty_cells (cell : Cell.t) ->
     Set.filter empty_cells ~f:(Cell.not_intersecting cell)
  )
;;

let set_if_only_one_option board =
  Set.fold (Board.empty_cells board) ~init:board ~f:(fun board cell ->
      let possible_values =
        possible_values board cell
        |> Set.to_list
      in
      match possible_values with
      | [] -> failwith "No possible values"
      | [ hd ] -> Board.set board hd cell 
      | _ :: _ -> board
    )
;;

let set_by_id board id =
  let cells = Cell.all_by_id id |> Cell.Set.of_list in
  let filled = Board.by_id board id in
  let empty = Cell.Set.diff cells (Cell.Set.of_list (Map.keys filled)) in
  let unused_values = Value.Set.diff Value.all (Value.Set.of_list (Map.data filled)) in
  let board, _empty =
    Value.Set.fold unused_values ~init:(board, empty) ~f:(fun (board, empty) value ->
      let possible_cells =
        possible_cells board empty value
        |> Cell.Set.to_list
      in
      match possible_cells with 
      | [] ->
        Board.print board;
        failwith "No possible cells"
      | [ cell ] ->
        let board = Board.set board value cell in
        let empty = Set.remove empty cell in
        (board, empty)
      | _ :: _ ->
        (board, empty)

    )
  in
  board
;;

let rec solve board =
  let empty_cells = Board.num_empty board in
  let board = set_if_only_one_option board in
  let board = List.fold Id.all ~init:board ~f:set_by_id in
  if Board.num_empty board < empty_cells
  then solve board
  else board
;;
