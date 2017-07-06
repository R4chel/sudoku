open Core

type t [@@deriving sexp]
include Comparable.S with type t := t

val all    : Set.t
val row    : t -> int
val column : t -> int
val square : t -> int 
val column_compare : t -> t -> int
val of_row_column_ids : row_id : Id.t -> column_id : Id.t -> t

