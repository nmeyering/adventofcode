open System
open System.IO

type Instruction =
    | Vertical of int
    | Horizontal of int

let parseRule (input: string) =
    match input.Split(" -> ") |> Array.map Seq.toArray with
    | [| [| a; b |]; [| c |] |] -> ((a, b), c)
    | _ -> failwith "invalid rule"

let template, rules =
    File.ReadLines(@"day14/input.txt")
    |> Seq.toList
    |> fun lines -> List.splitAt (List.findIndex ((=) "") lines) lines
    |> fun (template, rules) -> (template |> List.head |> Seq.toList, rules |> List.skip 1 |> List.map parseRule)

let produce (rules: ((char * char) * char) list) x y : char option =
    rules
    |> List.tryFind (fun ((a, b), _) -> a = x && b = y)
    |> Option.map snd

let apply (rules: ((char * char) * char) list) (template: char list): char list =
    let productions =
        template
        |> List.windowed 2
        |> List.map (function
            | [ a; b ] -> produce rules a b
            | _ -> None)

    let intersperse (p: char option, t: char) : char list =
        match p with
        | Some c -> [ c; t ]
        | None -> [ t ]

    let h :: rest = template
    List.zip productions rest
    |> List.collect intersperse
    |> fun r -> h :: r

let rec repeat n f x =
    match n with
    | 0 -> x
    | _ -> repeat (n - 1) f (f x)

template 
|> repeat 10 (apply rules)
|> List.countBy id
|> List.map snd
|> fun xs -> let min = List.min xs in let max = List.max xs in max - min
|> printfn "part 1: %A"

let addOrIncrement k (n: bigint) m =
    if Map.containsKey k m
        then Map.change k (Option.map (fun t -> t + n)) m
        else Map.add k n m

let apply2 (rules: ((char * char) * char) list) (pairs: Map<char * char, bigint>) (counts: Map<char, bigint>): Map<char * char, bigint> * Map<char, bigint> =
    let cs =
        pairs
        |> Map.toSeq
        |> Seq.choose (fun ((x, y), _) ->
            produce rules x y |> Option.map (fun c -> x, c, y)
        )
    let pairs' =
        Seq.fold (fun m (x, c, y) ->
            let n = Map.find (x, y) pairs
            m
            |> Map.change (x, y) (Option.map (fun t -> t - n))
            |> addOrIncrement (x, c) n
            |> addOrIncrement (c, y) n
        ) pairs cs
    let counts' =
        Seq.fold (fun m (x, c, y) ->
            let n = Map.find (x, y) pairs
            m |> addOrIncrement c n
        ) counts cs
    pairs', counts'

let pairs =
    template
    |> List.windowed 2
    |> List.map (function | [ a; b ] -> (a, b) | _ -> failwith "invalid window")
    |> List.countBy id
    |> List.map (fun (x, y) -> (x, bigint y))
    |> Map

let counts =
    template
    |> List.countBy id
    |> List.map (fun (x, y) -> (x, bigint y))
    |> Map

let rec applyN n rules pairs counts =
    if n = 0 then (pairs, counts) else
    let pairs', counts' = apply2 rules pairs counts
    applyN (n - 1) rules pairs' counts'

applyN 40 rules pairs counts
|> snd
|> Map.toList
|> List.map snd
|> fun xs -> let min = List.min xs in let max = List.max xs in max - min
|> printfn "part 2: %A"