open Core

type t = Value.t Cell.Map.t

let by_id t (id : Id.t) =
  Map.filter_keys t ~f:(fun (cell : Cell.t) ->
      match id with
      | Row r    -> Cell.row    cell = r
      | Column c -> Cell.column cell = c
      | Square s -> Cell.square cell = s
    )
;;

let validate_by_id t id =
  by_id t id
  |> Cell.Map.data 
  |> List.contains_dup
  |> not
;;

let cells_filled t =
  Cell.Map.length t 
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
  Out_channel.newline stdout;
  List.iter Id.rows ~f:(fun row_id ->
    List.map Id.columns ~f:(fun column_id ->
      match Cell.Map.find t (Cell.of_row_column_ids ~row:row_id ~column:column_id) with
      | Some value -> Int.to_string value
      | None -> "_"
    )
    |> String.concat ~sep:" "
    |> Out_channel.output_string stdout;
    Out_channel.newline stdout
    )
;;

let new_board = Cell.Map.empty
;;

let set t value ~row_id ~column_id =
  assert (Value.valid value);
  Cell.Map.add t ~key:(Cell.of_row_column_ids row_id column_id) ~data:value
;;
