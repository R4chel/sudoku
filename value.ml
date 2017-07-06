open Core

module T = struct
  type t = int [@@deriving sexp, compare]
  let compare = compare
  let hash = Hashtbl.hash
end
include T
include Hashable.Make (T)
include Comparable.Make(T)
let low = 1
let high = Id.n + 1
let all = List.range low high |> Set.of_list
let valid t = Int.between t ~low ~high
