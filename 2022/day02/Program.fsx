open System
open System.IO

let inputs =
    File.ReadLines(@"input.txt")
    |> Seq.toList
    
// let inputs = ["A Y";"B X";"C Z"]

type Move = R | P | S
type Game = Move * Move

let parseLeft (x: string) =
    match x with 
        | "A" -> R
        | "B" -> P
        | "C" -> S
        | _ -> failwith "invalid left move"

let parseRight (x: string) =
    match x with 
        | "X" -> R
        | "Y" -> P
        | "Z" -> S
        | _ -> failwith "invalid right move"

let parseGame (x: string) = 
    let moves = x.Split ' '
    match moves with
        | [| a; b |] -> (parseLeft a, parseRight b)
        | _ -> failwith "invalid game"

let games = inputs |> List.map parseGame

let score (g: Game): int =
    match g with
    | (R, R) -> 1 + 3
    | (P, R) -> 1 + 0
    | (S, R) -> 1 + 6
    | (R, P) -> 2 + 6
    | (P, P) -> 2 + 3
    | (S, P) -> 2 + 0
    | (R, S) -> 3 + 0
    | (P, S) -> 3 + 6
    | (S, S) -> 3 + 3

let answer1 = games |> List.map score |> List.sum

printfn "%A" answer1

type Outcome = X | Y | Z
type Strategy = Move * Outcome

let outcome (x: string): Outcome =
    match x with
    | "X" -> X
    | "Y" -> Y
    | "Z" -> Z
    | _ -> failwith "invalid outcome"

let parseStrat (x: string): Strategy = 
    let moves = x.Split ' '
    match moves with
        | [| a; b |] -> (parseLeft a, outcome b)
        | _ -> failwith "invalid game"
        
let scoreStrat (s: Strategy): int =
    match s with
    | (R, X) -> 0 + 3
    | (P, X) -> 0 + 1
    | (S, X) -> 0 + 2
    | (R, Y) -> 3 + 1
    | (P, Y) -> 3 + 2
    | (S, Y) -> 3 + 3
    | (R, Z) -> 6 + 2
    | (P, Z) -> 6 + 3
    | (S, Z) -> 6 + 1
    
let answer2 = inputs |> List.map parseStrat |> List.sumBy scoreStrat

printfn "%A" answer2