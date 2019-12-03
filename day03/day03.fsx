open System.IO
open System.Collections.Generic

type Command = R of int | U of int | L of int | D of int

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let parseCommand (cmd: string) =
    match cmd with
    | Prefix "R" steps -> R (int steps)
    | Prefix "L" steps -> L (int steps)
    | Prefix "U" steps -> U (int steps)
    | Prefix "D" steps -> D (int steps)
    | _ -> failwith "unknown command"
    

let chopOffTwo = function
                | [x; y;] -> x, y
                | _ -> failwith "fewer than two elements"

let wire1, wire2 = 
    File.ReadLines(@"day03/input.txt")
         |> Seq.map (fun csv -> csv.Split [|','|]
             >> Array.map parseCommand
             >> Array.toList)
         |> Seq.toList
         |> chopOffTwo

let manhattanDistance (x, y) =
    abs x + abs y

type Point = int * int

let goSteps (cmd: Command) (point: Point): Point list =
    let x, y = point
    match cmd with
        | L steps -> List.map (fun n -> (x - n, y)) [steps .. -1 .. 1]
        | R steps -> List.map (fun n -> (x + n, y)) [steps .. -1 .. 1]
        | U steps -> List.map (fun n -> (x, y + n)) [steps .. -1 .. 1]
        | D steps -> List.map (fun n -> (x, y - n)) [steps .. -1 .. 1]

let traceWire wire : Point list =
    match wire with 
    | [] -> []
    | (head :: tail) ->
        let folder (s: Point list) cmd = goSteps cmd (List.head s) @ s
        List.fold folder (goSteps head (0, 0)) tail

let intersection (left: 'a list) (right: 'a list) =
    let hash = HashSet< 'a >(right, HashIdentity.Structural)
    List.filter hash.Contains left

let crossPoints = intersection (traceWire wire1) (traceWire wire2)

let answer1 = crossPoints |> List.map manhattanDistance |> List.min

let stepsToCrosspoints wire =
    let trace = traceWire wire
    let len = List.length trace
    List.map (fun c -> len - List.findIndex (fun x -> x = c) trace) crossPoints

let uncurry f (a, b) = f a b

let answer2 = List.zip (stepsToCrosspoints wire1) (stepsToCrosspoints wire2)
              |> List.map (uncurry (+))
              |> List.min
