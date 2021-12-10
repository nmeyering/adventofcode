open System
open System.IO

let inputLines = File.ReadLines(@"day05/input.txt")

type Line = { X1: int; Y1: int; X2: int; Y2: int }

let parseLine (input: string) : Line =
    let points =
        input.Split(" -> ")
        |> Array.map (fun point -> point.Split(",") |> Array.map int)

    { X1 = points.[0].[0]
      Y1 = points.[0].[1]
      X2 = points.[1].[0]
      Y2 = points.[1].[1] }

let lines = inputLines |> Seq.map parseLine

let coveredPoints (line: Line) : (int * int) list =
    match line with
    | line when line.X1 = line.X2 && line.Y1 <= line.Y2 ->
        let x = line.X1
        [ line.Y1 .. line.Y2 ] |> List.map (fun y -> (x, y))
    | line when line.X1 = line.X2 && line.Y1 > line.Y2 ->
        let x = line.X1
        [ line.Y2 .. line.Y1 ] |> List.map (fun y -> (x, y))
    | line when line.Y1 = line.Y2 && line.X1 <= line.X2 ->
        let y = line.Y1
        [ line.X1 .. line.X2 ] |> List.map (fun x -> (x, y))
    | line when line.Y1 = line.Y2 && line.X1 > line.X2 ->
        let y = line.Y1
        [ line.X2 .. line.X1 ] |> List.map (fun x -> (x, y))
    | _ ->
        let len = abs (line.X1 - line.X2)
        let dx = if line.X1 < line.X2 then 1 else -1
        let dy = if line.Y1 < line.Y2 then 1 else -1
        [ 0 .. len ] |> List.map (fun i -> line.X1 + dx * i, line.Y1 + dy * i)

lines
|> Seq.map coveredPoints
|> Seq.concat 
|> Seq.countBy id
|> Seq.where (fun (p, count) -> count > 1)
|> Seq.length