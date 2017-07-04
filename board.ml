open Core

type t = Cell.t List.t

let by_id t (id : Id.t) =
  List.filter t ~f:(fun (cell : Cell.t) ->
      match id with
      | Row r -> cell.row = r
      | Column c -> cell.column = c
      | Square s -> Cell.square cell = s
    )
;;

let validate_by_id t id =
  let row = by_id t id in
  assert (List.length row = Id.n);
  List.filter_map row ~f:Cell.value
  |> List.contains_dup
  |> not
;;

let cells_filled t =
  List.count t ~f:(fun (cell : Cell.t) -> Option.is_some cell.value)
;;

let validate t =
  List.for_all Id.all ~f:(fun id ->
      validate_by_id t id
  )
;;

let complete t =
  validate t && cells_filled t = Id.n * Id.n
;;

let print t = 
  List.iter Id.rows ~f:(fun row_id ->
    by_id t row_id
    |> List.sort ~cmp:(fun (cell1 : Cell.t) cell2 -> Int.compare cell1.column cell2.column)
    |> List.map ~f:Cell.to_string
    |> String.concat ~sep:" "
    |> Out_channel.output_string stdout;
    Out_channel.newline stdout
    )
;;


let () =
  let new_board = Cell.all_cells in
  print new_board
