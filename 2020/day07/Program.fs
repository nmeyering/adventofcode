open System
open System.IO

type Rule = {
    BagType: string;
    Allowed: (string * int) list
}

let lines = File.ReadAllLines(@"day07/input.txt")
let parseRule (line: string) =
    let words = line.Split " "
    let key = words |> Seq.take 2
    let value = words |> Seq.skip 4
    let parseRule' (xs: string list): (string * int) =
            (xs |> List.item 1) + " " + (xs |> List.item 2), (xs |> List.item 0 |> int)
    let allowed = value |> Seq.toList |> List.chunkBySize 4
    {
        BagType = String.Join (" ", key)
        Allowed =
         allowed
         |> fun xs -> if List.length xs = 1 && (xs |> List.head |> List.length) = 3 then [] else List.map parseRule' xs
    }

let rules = lines |> Seq.toList |> List.map parseRule
// let rules = [
//     "light red bags contain 1 bright white bag, 2 muted yellow bags."
//     "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
//     "bright white bags contain 1 shiny gold bag."
//     "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
//     "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
//     "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
//     "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
//     "faded blue bags contain no other bags."
//     "dotted black bags contain no other bags." ] |> List.map parseRule

let sequenceOption l = 
    if Seq.contains None l then None
    else Some (Seq.map Option.get l)

let reachable (rules: Rule seq): Map<string, string Set> =
    let rec folder map rule =
        match rule with
        | { BagType = _; Allowed = [] } -> map
        | { BagType = b; Allowed = (h, _) :: t} ->
            let map' =
                match Map.tryFind h map with
                | None -> Map.add h (set [b]) map
                | Some s -> let set' = Set.add b s in Map.change h (fun _ -> Some set') map
            folder map' { BagType = b; Allowed = t }
    rules |> Seq.fold folder Map.empty

let r1 = reachable rules

let close' (k, t) =
    let union' s = if Seq.isEmpty s then Set.empty else Seq.reduce Set.union s
    let t' =
        t
        |> Seq.map (  (fun t' -> Map.tryFind t' r1)
                   >> (function | None -> set [] | Some s -> s))
        |> union' 
    (k, t')

let close r =
    r
    |> Map.toSeq
    |> Seq.map close'
    |> Seq.filter (fun (k, s) -> not <| Seq.isEmpty s)
    |> Map.ofSeq

let merge (m1: Map<'a, Set<'a>>) (m2: Map<'a, Set<'a>>): Map<'a, Set<'a>> =
    let folder acc key value =
        match Map.tryFind key acc with
        | Some _ -> Map.change key (Option.map (fun x -> Set.union x value)) acc
        | None -> Map.add key value acc
    Map.fold folder m1 m2

let answer1 = 
    r1
    |> Seq.unfold (fun m -> if Map.isEmpty m then None else Some (m, close m))
    |> Seq.reduce merge
    |> Map.find "shiny gold"
    |> Set.count

let findRule bag =
    rules |> Seq.find (fun r -> r.BagType = bag)

let contents (bag, n) =
    bag |> findRule |> fun r -> r.Allowed |> List.map (fun (s, n') -> (s, n * n'))

let generator (bags: (string * int) list): (((string * int) list) * ((string * int) list)) option =
    let bags' =
        bags
        |> List.collect contents
    match bags with
        | [] -> None
        | _ -> Some (bags, bags')
in List.unfold generator [("shiny gold", 1)]
|> List.skip 1
|> List.reduce (@)
|> List.sumBy snd