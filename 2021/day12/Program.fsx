open System
open System.IO

type Cave = Start | End | Big of string | Small of string

let parse (line: string): Cave * Cave =
    let parse' (s: string): Cave =
        match s with
        | "start" -> Start
        | "end" -> End
        | _ ->
            match (s |> Seq.toList) with
            | c :: _ when Char.IsUpper c -> Big s
            | c :: _ when Char.IsLower c -> Small s
            | _ -> failwith "invalid cave name"
    
    match line.Split('-') with
    | [| src; dest |] -> parse' src, parse' dest
    | _ -> failwith "invalid line"

let graph =
    File.ReadLines(@"2021/day12/input.txt")
    |> Seq.map parse
    |> Seq.toList

let smallCaveAllowed1 c path =
    path
    |> List.filter ((=)c)
    |> List.length
    |> (fun x -> x < 1)

let isSmall = function Small _ -> true | _ -> false

let smallCaveAllowed2 c path =
    let counts =
        path
        |> List.filter isSmall
        |> List.countBy id
    match counts |> List.tryFind (fun (x, _) -> x = c) with
    | None -> true
    | Some (_, 1) -> 
        counts
        |> List.filter (snd >> (fun x -> x > 1))
        |> List.isEmpty
    | _ -> false

let allowedEdges smallCaveAllowed (path: Cave list) (edges: (Cave * Cave) list): (Cave * Cave) list =
    let allowed (h :: rest as path) (a, b as edge) =
        match edge with
        | x, End
        | x, Big _ when x = h -> Some (a, b)
        | x, Small _ when x = h && smallCaveAllowed b path -> Some (a, b)
        | End, x
        | Big _, x when x = h -> Some (b, a)
        | Small _, x when x = h && smallCaveAllowed a path -> Some (b, a)
        | _ -> None

    match path with
    | [] -> failwith "empty path"
    | _ -> edges |> List.choose (allowed path)

let extend s (graph: (Cave * Cave) list) (path: Cave list): Cave list list =
    match path with
    | [] -> failwith "empty path"
    | End :: _ -> [path]
    | _ ->
    allowedEdges s path graph
    |> List.map snd
    |> List.map (fun dst -> dst :: path)

let allPaths s graph =
    let rec allPaths' paths =
        let unfinished = paths |> List.filter (fun (h :: path) -> h <> End) |> List.length
        if unfinished = 0 then paths else
        paths |> List.collect (extend s graph) |> allPaths'
    allPaths' [[Start]]

allPaths smallCaveAllowed1 graph |> List.length |> printfn "part 1: %A"
allPaths smallCaveAllowed2 graph |> List.length |> printfn "part 2: %A"