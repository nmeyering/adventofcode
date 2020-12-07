open System
open System.IO

let groupBy' f =
    let grouper (acc : string list list) (x: string): (string list list) =
        match acc with
        | [] -> [[x]]
        | h :: t -> if f x then ([] :: h :: t) else ((x :: h) :: t)
    List.fold grouper [] >> List.rev

let lines = File.ReadAllLines(@"day06/input.txt")

let answers =
    lines
    |> Seq.toList
    |> groupBy' ((=)"")

let answer1 =
    answers
    |> Seq.map (Seq.concat >> Seq.distinct >> Seq.length)
    |> Seq.sum

let everyoneYes (answers': string seq) =
    answers'
    |> Seq.map set
    |> Seq.reduce Set.intersect

let answer2 =
    answers
    |> Seq.map everyoneYes
    |> Seq.sumBy Set.count