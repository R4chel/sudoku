open Core

type t =
  | Row    of int
  | Column of int
  | Square of int

let n = 9

let rows =
  List.map (List.range 0 n) ~f:(fun i -> Row i)

let columns =
  List.map (List.range 0 n) ~f:(fun i -> Column i)

let squares =
  List.map (List.range 0 n) ~f:(fun i -> Square i)
   
let all =
  rows @ columns @ squares

let values = List.range 1 (n + 1)
