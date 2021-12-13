open System
open System.IO

type Instruction = Vertical of int | Horizontal of int

let parseInstruction (input: string): Instruction =
    let instruction = input.Split().[2]
    match instruction.Split("=") with
    | [| "y"; n |] -> Horizontal (int n)
    | [| "x"; n |] -> Vertical (int n)
    | _ -> failwith "invalid instruction"

let parsePoint (input: string): int * int =
    match input.Split(",") |> Array.map int with
    | [| x; y |] -> x, y
    | _ -> failwith "invalid point"

let points, instructions =
    File.ReadLines(@"2021/day13/input.txt")
    |> Seq.toList
    |> fun lines -> List.splitAt (List.findIndex ((=)"") lines) lines
    |> fun (ps, is) -> (ps |> List.map parsePoint, is |> List.skip 1 |> List.map parseInstruction)

let foldPaper (instruction: Instruction) ((x, y): int * int): int * int =
    match instruction with
    | Vertical n -> if x > n then (2 * n - x, y) else (x, y)
    | Horizontal n -> if y > n then (x, 2 * n - y) else (x, y)

points
|> List.map (foldPaper (List.head instructions))
|> set |> Set.count
|> printfn "part 1: %A"

let rec foldAll (instructions: Instruction list) (points: (int * int) list): (int * int) list =
    match instructions with
    | [] -> points |> set |> Set.toList
    | i :: r -> points |> List.map (foldPaper i) |> foldAll r

let picture = points |> foldAll instructions
let width = picture |> List.map fst |> List.max
let height = picture |> List.map snd |> List.max
[
    for y in [0..height] do
    yield [ for x in [0..width] do
            yield if List.contains (x, y) picture then '#' else '.'
    ] |> List.toArray |> fun cs -> new string(cs)
] |> printfn "part 2:\n %A"