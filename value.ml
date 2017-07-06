open Core

type t = int
let low = 1
let high = Id.n + 1
let all = List.range low high
let valid t = Int.between t ~low ~high
