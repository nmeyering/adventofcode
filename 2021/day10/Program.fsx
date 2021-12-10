open System.IO

let lines =
    File.ReadLines(@"2021/day10/input.txt")
    |> Seq.map (Seq.toArray >> List.ofArray)

let closing = [')'; ']'; '}'; '>']
let opening = ['('; '['; '{'; '<']

let isClosing c = List.contains c closing

let score c =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith "invalid char"

let folder ((stack, valid): char list * bool) (c: char): char list * bool =
    if not valid then stack, false else
    match stack with
    | [] -> [ c ], valid
    | last :: rest ->
        match last, c with
        | '(', ')'
        | '[', ']'
        | '<', '>'
        | '{', '}' -> rest, true
        | _, c when isClosing c -> c :: stack, false
        | _ -> c :: stack, true

let f cs =
    let stack, valid = Seq.fold folder ([], true) cs
    if valid then None else
        match stack with
        | [] -> None
        | h :: _ -> Some h

lines
|> Seq.map f
|> Seq.sumBy (function | Some c -> score c | _ -> 0)
|> printfn "part 1: %A"

let score2 cs: bigint =
    let score2' c = 1 + List.findIndex ((=)c) opening
    let folder (score: bigint) c =
        score * 5I + bigint (score2' c)
    List.fold folder 0I cs

lines
|> Seq.map (Seq.fold folder ([], true))
|> Seq.filter (fun (_, valid) -> valid)
|> Seq.map (fun (stack, _) -> score2 stack)
|> Seq.sort
|> Seq.toList
|> fun xs -> let middle = (List.length xs) / 2 in xs.[middle]