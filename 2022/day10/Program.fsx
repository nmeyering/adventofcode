open System
open System.IO

let inputs = File.ReadLines(@"day10/input.txt") |> Seq.toList

type Cmd = Nop | AddX of int

let parseInstr (line: string): Cmd =
    match line with
    | "noop" -> Nop
    | _ -> line.Split ' ' |> fun [|_; v|] -> AddX (int v)

let execute (cycle: int, x: int) (c: Cmd): (int * int) list =
    match c with
    | Nop -> [(cycle + 1, x)]
    | AddX v -> [(cycle + 2, v + x); (cycle + 1, x)]

let rec f (folder: 'x -> 'c -> 'x list) (acc: 'x list) (cs: 'c list): 'x list =
    match cs with
    | [] -> acc
    | c :: tail ->
    let x = List.head acc
    let acc' = List.append (folder x c) acc
    f folder acc' tail

let program = inputs |> List.map parseInstr

program |> f execute [(1,1)]
    |> List.rev
    |> List.skip 19
    |> List.chunkBySize 40
    |> List.map (List.head)
    |> List.sumBy (fun (c, x) -> c * x)
    |> printfn "Part 1: %A"

let draw (cycle: int, x: int, _) (c: Cmd): (int * int * char) list =
    let pixel c x = if abs (x - ((c - 1) % 40)) <= 1 then '#' else '.'
    let n c x = (c, x, pixel c x)
    match c with
    | Nop -> [n (cycle + 1) x]
    | AddX v -> [n (cycle + 2) (v + x); n (cycle + 1) x]

program |> f draw [(1,1,'#')]
|> List.rev
|> List.map (fun (_,_,p) -> p)
|> List.toArray
|> Array.chunkBySize 40
|> Array.map (fun s -> new String (s))
|> printfn "Part 2: \n %A"