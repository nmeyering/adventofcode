open System
open System.IO

let lines = File.ReadLines(@"day04/input.txt")

let numbers: int list =
    lines
    |> Seq.head
    |> (fun line -> line.Split ',')
    |> Seq.map int
    |> Seq.toList

type Square =
    | Marked of int
    | Unmarked of int

type Board = Square array

let parseBoard (input: string array) : Board =
    input
    |> Array.map (fun line -> line.Split() |> Array.where (fun x -> x <> ""))
    |> Array.concat
    |> Array.map int
    |> Array.map (fun n -> Unmarked n)

let boards =
    lines
    |> Seq.skip 1
    |> Seq.chunkBySize 6
    |> Seq.map (fun board -> board |> Array.skip 1)
    |> Seq.map parseBoard
    |> Seq.toList

let mark (number: int) (board: Board) : Board =
    board
    |> Array.map (fun square ->
        match square with
        | Unmarked x when x = number -> Marked number
        | x -> x)

let score (board: Board) : int =
    board
    |> Array.sumBy (function
        | Unmarked x -> x
        | _ -> 0)

let wins (board: Board) : bool =
    let winners =
        [
          [ 0; 1; 2; 3; 4 ]
          [ 5; 6; 7; 8; 9 ]
          [ 10; 11; 12; 13; 14 ]
          [ 15; 16; 17; 18; 19 ]
          [ 20; 21; 22; 23; 24 ]
          [ 0; 5; 10; 15; 20 ]
          [ 1; 6; 11; 16; 21 ]
          [ 2; 7; 12; 17; 22 ]
          [ 3; 8; 13; 18; 23 ]
          [ 4; 9; 14; 19; 24 ]
        //   diagonals
        //   [ 0; 6; 12; 18; 24 ]
        //   [ 4; 8; 12; 16; 20 ]
        ]

    let isMarked (square: Square) : bool =
        square
        |> function
            | Unmarked _ -> false
            | _ -> true

    let any =
        List.fold (fun state x -> state || x) false

    let allMarked (board: Board) (indices: int list) : bool =
        indices
        |> List.fold (fun state i -> state && (isMarked board.[i])) true

    winners |> List.map (allMarked board) |> any

let rec play (boards: Board list) (numbers: int list) : (Board * int) option =
    match numbers with
    | [] -> None
    | number :: tail ->
        let marked = boards |> List.map (mark number)
        let maybeWinner = marked |> Seq.tryFind wins

        match maybeWinner with
        | None -> play marked tail
        | Some winner -> Some(winner, number * score winner)

let answer1 = play boards numbers

let rec playToLose (boards: Board list) (numbers: int list) : (Board * int) option =
    match numbers with
    | [] -> None
    | number :: tail ->
        let losers =
            boards
            |> List.map (mark number)
            |> Seq.where (not << wins)
            |> Seq.toList

        match losers with
        | [] ->
            match boards with
            | [ lastLoser ] -> let last = mark number lastLoser in Some(last, number * score last)
            | _ -> None
        | _ -> playToLose losers tail

let answer2 = playToLose boards numbers