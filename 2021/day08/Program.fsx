open System.IO
open FSharp.Core

type Entry = {
    Patterns: string list
    Output: string list
}

let parseEntry (line: string): Entry =
    let ws = line |> fun s -> s.Split()
    let si = ws |> Array.findIndex (fun x -> x = "|")
    let p, o = ws |> Array.splitAt si
    {
        Patterns = p |> Array.toList
        Output = o |> Array.skip 1 |> Array.toList
    }


let entries =
    File.ReadLines(@"2021/day08/input.txt")
    |> Seq.toList
    |> List.map parseEntry

let guessDigit (pattern: string): int option =
    match pattern with
    | x when x.Length = 2 -> Some 1
    | x when x.Length = 4 -> Some 4
    | x when x.Length = 3 -> Some 7
    | x when x.Length = 7 -> Some 8
    | _ -> None

entries
    |> List.map (fun e -> e.Output)
    |> List.concat
    |> List.map guessDigit
    |> List.where (Option.isSome)
    |> List.length
    |> printfn "part 1: %A"

let inline (.-) (p: string) (q: string): string =
    let p' = set p
    let q' = set q
    new string (p' - q' |> Set.toArray |> Array.sort)

let inline (.*) (p: string) (q: string): string =
    let p' = set p
    let q' = set q
    new string (Set.intersect p' q' |> Set.toArray |> Array.sort)

let inline (.+) (p: string) (q: string): string =
    let p' = set p
    let q' = set q
    new string (Set.union p' q' |> Set.toArray |> Array.sort)

let inline (.=) (p: string) (q: string): bool =
    let p' = set p
    let q' = set q
    p' = q'

let toStr = fun cs -> new string (cs |> Set.toArray |> Array.sort)

let f (entry: Entry) =
    let p1 = entry.Patterns |> List.find (fun x -> x.Length = 2)
    let p4 = entry.Patterns |> List.find (fun x -> x.Length = 4)
    let p7 = entry.Patterns |> List.find (fun x -> x.Length = 3)
    let p8 = entry.Patterns |> List.find (fun x -> x.Length = 7)

    let eg = (p8 .- p4) .- p7
    let g = entry.Patterns |> List.map (fun p -> p .* eg) |> List.find (fun x -> (String.length x) = 1)
    let e = eg .- g
    let p9 = p8 .- e
    let c = entry.Patterns |> List.map (fun p -> p9 .- p) |> List.countBy id |> List.find (fun (p, n) -> p <> "" && n = 2) |> fst
    let p5 = p9 .- c
    let p6 = p5 .+ e
    let known = (set (List.map (set >> toStr) [p1;p4;p5;p6;p7;p8;p9]))
    let p023' = entry.Patterns |> List.map (set >> toStr) |> set
    let p023 = Set.difference p023' known |> Set.toList
    let p0 = p023 |> List.find (fun p -> (String.length p) = 6)
    let p3 = p023 |> List.find (fun p -> p .- p1 |> String.length |> (fun l -> l = 3))
    let p2 = p023 |> List.find (fun p -> p <> p3 && p <> p0)
    let table = [| p0; p1; p2; p3; p4; p5; p6; p7; p8; p9 |]

    let decode (x: string): int =
        table |> Array.findIndex (fun p -> p .= x)
    let xs = entry.Output |> List.map decode
    xs.[0] * 1000 + xs.[1] * 100 + xs.[2] * 10 + xs.[3]

entries |> List.sumBy f |> printfn "part 2: %A"