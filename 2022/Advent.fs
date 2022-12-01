module Advent

let splitOn f =
    let grouper (acc : string list list) (x: string): (string list list) =
        match acc with
        | [] -> [[x]]
        | h :: t -> if f x then ([] :: h :: t) else ((x :: h) :: t)
    Seq.fold grouper [] >> Seq.rev