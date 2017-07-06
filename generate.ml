open Core

let rec set_row (board : Board.t) row_id =
  let row = List.permute Id.values in
  let new_board : Board.t =
    List.fold2_exn Id.columns row ~init:board ~f:(fun board column_id value ->
      Board.set_by_ids board value ~row_id ~column_id
  )
  in
  if Board.validate new_board
  then new_board
  else set_row board row_id
;;

let make_board () =
  List.foldi Id.rows ~init:Board.new_board ~f:(fun i board row_id ->
      Out_channel.output_string stdout ("Starting row " ^ Int.to_string i);
      Out_channel.newline stdout;
      set_row board row_id 
    )
;;

let parse_one_board grid =
  List.fold2_exn  Id.rows grid ~init:Board.new_board ~f:(fun board row_id row ->
    let nums = 
      String.to_list row
      |> List.map ~f:Char.get_digit_exn
    in
    List.fold2_exn Id.columns nums ~init:board ~f:(fun board column_id num ->
        if num <> 0
        then Board.set_by_ids board num ~row_id ~column_id
        else board
      )
    )
;;

let rec parse_boards lines =
  match lines with
  | [] -> []
  | hd :: tl ->
    Out_channel.output_string stdout hd;
    let new_board, tl = List.split_n tl Id.n in
    parse_one_board new_board :: parse_boards tl 
;;

let read_boards =
  let file = "bin/p096_sudoku.txt" in
  let data = In_channel.read_lines file in
  let boards = parse_boards data in
  boards
;;

let get_board ?(index=0) () =
  let boards = read_boards in
  match List.nth boards index with
  | Some board -> board
  | None -> List.hd_exn boards
;;
