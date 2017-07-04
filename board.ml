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

let () =
  Out_channel.output_string stdout "hello world!"
