open System
open System.IO

let checkPassword (c: char) (min: int) (max: int) (password: string): bool =
    let count = Seq.sumBy (fun x -> if x = c then 1 else 0) password in
    count >= min && count <= max

let parseLine (line: string) =
    match line.Split ':' with
    | [| h; t |] -> Some (h, t)
    | _ -> None

let parseRule (rule: string) =
    match rule.Split ' ' with
    | [| h; t |] -> Some (h, t)
    | _ -> None

let splitLimits (limits: string) =
    match limits.Split '-' with
    | [| min; max |] -> Some (min, max)
    | _ -> None

let parseLimit (limit: string) =
    match Int32.TryParse limit with
    | (true, x) -> Some x
    | _ -> None

let parseLimits (min: string, max: string) =
    match (parseLimit min, parseLimit max) with
    | (Some x, Some y) -> Some (x, y)
    | _ -> None

let lines = File.ReadAllLines(@"day02/input.txt") |> Array.toList

let checkPassword2 (c: char) (min: int) (max: int) (password: string): bool =
    let first' = Seq.tryItem (min - 1) password
    let second' = Seq.tryItem (max - 1) password
    match first', second' with
        | Some first, Some second -> first = c && second <> c || first <> c && second = c
        | _ -> false

let check' checker line =
    let entry = parseLine line
    let password = entry |> Option.map snd |> Option.map (fun x -> x.Trim ())
    let rule = entry |> Option.map fst |> Option.bind parseRule 
    let c = rule |> Option.map snd |> Option.map char
    let limits = rule
              |> Option.map fst
              |> Option.bind splitLimits 
              |> Option.bind parseLimits
    let min = limits |> Option.map fst
    let max = limits |> Option.map snd

    match (c, min, max, password) with
    | (Some c', Some min', Some max', Some password) -> checker c' min' max' password
    | _ -> false

let answer1 = lines |> List.filter (check' checkPassword) |> List.length
let answer2 = lines |> List.filter (check' checkPassword2) |> List.length