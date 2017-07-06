open Core

let possible_values board cell =
  let row_id    = Id.Row    (Cell.row    cell) in
  let column_id = Id.Column (Cell.column cell) in
  let square_id = Id.Square (Cell.square cell) in
  let row    = Board.by_id board row_id in
  let column = Board.by_id board column_id in
  let square = Board.by_id board square_id in
  let all_intersecting_values = row @ column @ square in
  List.fold all_intersecting_values ~init:Value.all ~f:(fun acum num ->
      Value.Set.remove acum num
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

let rec solve board =
  Out_channel.output_string stdout "Solving ... ";
  Out_channel.newline stdout;
  Board.print board;
  Out_channel.newline stdout;
  let empty_cells = Board.num_empty board in
  let board = set_if_only_one_option board in
  if Board.num_empty board < empty_cells
  then solve board
  else board
;;
