open Core

let () =
  let boards = Generate.read_boards in
  let num_empty =

  List.fold boards ~init:0 ~f:(fun sum board ->
  let board = Solver.solve board in
  let empties = Board.num_empty board in
  Out_channel.output_string stdout (Int.to_string empties);
  Out_channel.newline stdout;
  sum + empties
  )
  in
  Out_channel.newline stdout;
  Out_channel.output_string stdout (Int.to_string num_empty);
  Out_channel.newline stdout;
;;
