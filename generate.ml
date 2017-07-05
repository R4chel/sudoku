open Core

let rec set_row (board : Board.t) row_id =
  let row = List.permute (List.range 0 Id.n) in
  let new_board : Board.t =
    List.fold2_exn Id.columns row ~init:board ~f:(fun board column_id value ->
      Board.set board value row_id column_id
  )
  in
  if Board.validate new_board
  then new_board
  else
    (
      Out_channel.output_string stdout "Unsuccessful";
      set_row board row_id
    )


let make_board =
  List.foldi Id.rows ~init:Board.new_board ~f:(fun i board row_id ->
      Out_channel.output_string stdout ("Starting row " ^ Int.to_string i);
      Out_channel.newline stdout;
      set_row board row_id 
    )
;;

let () =
  let board = make_board in
  Out_channel.newline stdout;
  Out_channel.newline stdout;
  Board.print board
