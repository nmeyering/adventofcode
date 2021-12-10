open System
open System.IO

module Option =
    let (>>=) r f = Option.bind f r
    let rtn v     = Some v

    let traverseList f ls =
        let folder head tail = f head >>= (fun h -> tail >>= (fun t -> h::t |> rtn))
        List.foldBack folder ls (rtn List.empty)
    let sequenceList   ls = traverseList id ls

let lines = File.ReadLines(@"day02/input.txt")

type Command =
    Forward of int
    | Up of int
    | Down of int

let parseCommand (line: string): Command option =
    match line.Split ' ' with
    | [| command; amount |] ->
        let n = int amount
        match command with
        | "forward" -> Forward n |> Some
        | "down" -> Down n |> Some
        | "up" -> Up n |> Some
        | _ -> None
    | _ -> None

let commands = lines |> Seq.map parseCommand

let dive (x, y) command =
    match command with
    | Forward n -> (x + n, y)
    | Up n -> (x, y - n)
    | Down n -> (x, y + n)

let pos =
    commands
    |> Seq.toList
    |> Option.sequenceList
    |> Option.map (List.fold dive (0, 0))

let answer: int option =
    pos
    |> Option.map (fun (x, y) -> x * y)

printfn $"answer part 1: %A{answer}"