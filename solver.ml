open Core
module CI = Base.Container_intf

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

let possible_cells_of_two_values board empty_cells value1 value2 =
  Set.union
    (possible_cells board empty_cells value1)
    (possible_cells board empty_cells value2)
;;

let possible_cells_of_three_values board empty_cells value1 value2 value3 =
  Set.union
    (possible_cells board empty_cells value1)
    (possible_cells board empty_cells value2)
  |> Set.union
    (possible_cells board empty_cells value3)
;;

let set_if_only_one_option_exn board =
  Set.fold (Board.empty_cells board) ~init:board ~f:(fun board cell ->
      let possible_values =
        possible_values board cell
        |> Set.to_list
      in
      match possible_values with
      | [] -> failwith "No possible values"
      | [ value ] -> Board.set board cell value
      | _ :: _ -> board
    )
;;

let set_if_only_one_option board =
  Set.fold_until (Board.empty_cells board) ~init:board ~f:(fun board cell ->
      let possible_values =
        possible_values board cell
        |> Set.to_list
      in
      match possible_values with
      | [] -> CI.Continue_or_stop.Stop ()
      | [ value ] -> Board.set board cell value |> CI.Continue_or_stop.Continue
      | _ :: _ -> CI.Continue_or_stop.Continue board
    )
;;

let filled_and_empty_by_id board id =
  let cells = Cell.all_by_id id |> Cell.Set.of_list in
  let filled = Board.by_id board id in
  let empty = Cell.Set.diff cells (Cell.Set.of_list (Map.keys filled)) in
  filled, empty
;;

let set_by_subset board empty unused_values =
  let board, _empty =
    Value.Set.fold unused_values ~init:(board, empty) ~f:(fun (board, empty) value ->
      let possible_cells =
        possible_cells board empty value
        |> Cell.Set.to_list
      in
      match possible_cells with 
      | [] ->
        failwith "No possible cells"
      | [ cell ] ->
        let board = Board.set board cell value in
        let empty = Set.remove empty cell in
        (board, empty)
      | _ :: _ ->
        (board, empty)
    )
  in
  board

let set_by_id board id =
  (* Out_channel.output_string stdout ("Set by id: " ^ (Id.to_string id)); *)
  let filled, empty = filled_and_empty_by_id board id in
  let unused_values = Value.Set.diff Value.all (Value.Set.of_list (Map.data filled)) in
  let num_empty = Board.num_empty board in
  let board = set_by_subset board empty unused_values in
  if Set.length empty = 1 then assert (num_empty = 1 + Board.num_empty board);
  board
;;

let rec n_choose_2 l =
  match l with
  | [] | [ _ ] -> []
  | hd1 :: hd2 :: tl ->  (hd1, hd2) :: n_choose_2 (hd1::tl) @ n_choose_2 (hd2::tl)
;;

let rec n_choose_3 l =
  match l with
  | [] | [ _ ] | _ :: [ _ ]-> []
  | hd1 :: hd2 :: hd3 :: tl ->
    (hd1, hd2, hd3) :: n_choose_3 (hd1::tl) @ n_choose_3 (hd2::tl) @ n_choose_3 (hd3 :: tl)
;;


let two_by_two board id =
  (* Out_channel.output_string stdout ("Two by two: " ^ (Id.to_string id)); *)
  (* Out_channel.newline stdout; *)
  let filled, empty = filled_and_empty_by_id board id in
  let unused_values = Value.Set.diff Value.all (Value.Set.of_list (Map.data filled)) in
  let possible_value_pairs = n_choose_2 (Set.to_list unused_values) in
  let board : (Board.t, Board.t) CI.Finished_or_stopped_early.t = 
    List.fold_until possible_value_pairs ~init:board ~f:(fun board (v1, v2) ->
      let possible_cells = possible_cells_of_two_values board empty v1 v2 in
      match Set.to_list possible_cells with
      | c1 :: c2 :: [] ->
        let unused_values = Set.remove unused_values v1 in
        let unused_values = Set.remove unused_values v2 in
        let empty = Set.remove empty c1 in
        let empty = Set.remove empty c2 in
        let num_empty = Board.num_empty board in
        let board = set_by_subset board empty unused_values in
        if Board.num_empty board < num_empty
        then CI.Continue_or_stop.Stop board
        else CI.Continue_or_stop.Continue board
      | _ -> CI.Continue_or_stop.Continue board
    )
  in
  match board with
  | Finished board
  | Stopped_early board -> board 
;;

let three_by_three board id =
  (* Out_channel.output_string stdout ("Two by two: " ^ (Id.to_string id)); *)
  (* Out_channel.newline stdout; *)
  let filled, empty = filled_and_empty_by_id board id in
  let unused_values = Value.Set.diff Value.all (Value.Set.of_list (Map.data filled)) in
  let possible_value_pairs = n_choose_3 (Set.to_list unused_values) in
  let board : (Board.t, Board.t) CI.Finished_or_stopped_early.t = 
    List.fold_until possible_value_pairs ~init:board ~f:(fun board (v1, v2, v3) ->
      let possible_cells = possible_cells_of_three_values board empty v1 v2 v3 in
      match Set.to_list possible_cells with
      | c1 :: c2 :: c3 :: [] ->
        let unused_values = Set.remove unused_values v1 in
        let unused_values = Set.remove unused_values v2 in
        let unused_values = Set.remove unused_values v3 in
        let empty = Set.remove empty c1 in
        let empty = Set.remove empty c2 in
        let empty = Set.remove empty c3 in
        let num_empty = Board.num_empty board in
        let board = set_by_subset board empty unused_values in
        if Board.num_empty board < num_empty
        then CI.Continue_or_stop.Stop board
        else CI.Continue_or_stop.Continue board
      | _ -> CI.Continue_or_stop.Continue board
    )
  in
  match board with
  | Finished board -> board
  | Stopped_early board -> board 
;;

let rec solve board =
  let empty_cells = Board.num_empty board in
  let board = set_if_only_one_option_exn board in
  let board = List.fold Id.all ~init:board ~f:set_by_id in
  let board = List.fold Id.all ~init:board ~f:two_by_two in
  let board = List.fold Id.all ~init:board ~f:three_by_three in
  if Board.num_empty board < empty_cells
  then solve board
  else board
;;

let rec brute_force board =
  match set_if_only_one_option board with
  | CI.Finished_or_stopped_early.Stopped_early () -> None
  | CI.Finished_or_stopped_early.Finished board ->
    let empties = Board.empty_cells board in
    match Set.choose empties with
    | None -> Some board
    | Some cell ->
      let possible_values = possible_values board cell in
      Value.Set.find_map possible_values ~f:(fun value ->
          let new_board = Board.set board cell value in
          brute_force new_board
        )
;;

let solve_with_brute_force board =
  let board = solve board in
  match brute_force board with
  | Some board -> board
  | None -> failwith "oops"
;;
