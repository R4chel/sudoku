open Core

let () =
  let boards = Generate.read_boards () in
  Board.print (List.hd_exn boards)
;;
