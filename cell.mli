open Core

type t [@@deriving sexp]
include Comparable.S with type t := t

val all : t List.t
val row : t -> int
val column : t -> int
val square : t -> int 
val column_compare : t -> t -> int
val of_row_column_ids : row : Id.t -> column : Id.t -> t

