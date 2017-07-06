open Core

let () =
  let board = Generate.get_board () in
  let board = Solver.solve board in
  Board.print board;
;;
