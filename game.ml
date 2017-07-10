open Core

let () =
  let boards = Generate.read_boards in
  let num_empty =
  List.foldi boards ~init:0 ~f:(fun i sum board ->
    Out_channel.output_string stdout ("Grid " ^ Int.to_string i ^ "... ");
    Out_channel.newline stdout;
    let board = Solver.solve_with_brute_force board in
    assert (Board.complete board);
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
