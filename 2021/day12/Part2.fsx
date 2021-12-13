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

let out graph cave =
    let forwardEdges = graph |> List.filter (fst >> (=)cave)
    let backEdges =
        graph
        |> List.choose (fun edge -> 
            match edge with
            | (Big b, a) when a = cave -> Some (cave, Big b)
            | _ -> None
        )
    List.append forwardEdges backEdges

let isSmall = function Small _ -> true | _ -> false
let smallCaveAllowed path c =
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

let allowedEdges2 (path: Cave list) (edges: (Cave * Cave) list): (Cave * Cave) list =
    let allowed (h :: rest as path) (a, b as edge) =
        match edge with
        | x, End
        | x, Big _ when x = h -> Some (a, b)
        | x, Small _ when x = h && smallCaveAllowed path b -> Some (a, b)
        | End, x
        | Big _, x when x = h -> Some (b, a)
        | Small _, x when x = h && smallCaveAllowed path a -> Some (b, a)
        | _ -> None

    match path with
    | [] -> failwith "empty path"
    | _ -> edges |> List.choose (allowed path)

let extend2 (graph: (Cave * Cave) list) (path: Cave list): Cave list list =
    match path with
    | [] -> failwith "empty path"
    | End :: _ -> [path]
    | _ ->
    allowedEdges2 path graph
    |> List.map snd
    |> List.map (fun dst -> dst :: path)

let allPaths2 graph =
    let rec allPaths' paths =
        let unfinished = paths |> List.filter (fun (h :: path) -> h <> End) |> List.length
        if unfinished = 0 then paths else
        paths |> List.collect (extend2 graph) |> allPaths'
    allPaths' [[Start]]

allPaths2 graph |> List.length |> printfn "part 2: %A"