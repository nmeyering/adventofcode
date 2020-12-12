open System.IO
open System

let input = File.ReadAllLines ("day12/input.txt")
// let input = [
//  "F10"
//  "N3"
//  "F7"
//  "R90"
//  "F11"   
// ]

type Direction = N | E | S | W

type State = {
    Dir: Direction
    X: int
    Y: int
}

let rotate (startDir: Direction) (degrees: int): Direction =
    let angle dir =
        match dir with
        | N -> 90
        | E -> 0
        | S -> 270
        | W -> 180
    let direction degrees =
        match degrees with
        | 90 -> N
        | 0 -> E
        | 270 -> S
        | 180 -> W
        | _ -> failwith "invalid angle"
    direction <| (degrees + angle startDir) % 360

let move (state: State) (dir: Direction) (amount: int): State =
    match dir with
    | N -> { state with Y = state.Y - amount }
    | S -> { state with Y = state.Y + amount }
    | E -> { state with X = state.X + amount }
    | W -> { state with X = state.X - amount }

let step state action amount =
    match action with
    | 'N' -> move state N amount
    | 'S' -> move state S amount
    | 'E' -> move state E amount
    | 'W' -> move state W amount
    | 'L' -> { state with Dir = rotate state.Dir amount }
    | 'R' -> { state with Dir = rotate state.Dir (360 - amount) }
    | 'F' -> move state state.Dir amount
    | _ -> failwith "invalid action"

let manhattan x y = abs x + abs y

let parseAction (line : string) =
    let a = line.[0]
    let rest = line.Substring 1
    (a, int rest)

let answer1 =
    input
    |> Seq.toList
    |> List.map parseAction
    |> List.fold (fun state (action, amount) -> step state action amount) { Dir = E; X = 0; Y = 0 }
    |> fun s -> printfn "%A" s; manhattan s.X s.Y

type WaypointState = {
    X: int
    Y: int
    Wx: int
    Wy: int
}

let rotateWaypoint (state: WaypointState) (amount: int): WaypointState =
    match amount with
    | 180 -> { state with Wx = -state.Wx; Wy = -state.Wy }
    | 90 -> { state with Wx = -state.Wy; Wy = state.Wx } // (10 -4) -> (4 10)
    | 270 -> { state with Wx = state.Wy; Wy = -state.Wx } // (10 -4) -> (-4 -10)
    | _ -> state

let waypointStep (state: WaypointState) action amount =
    match action with
    | 'N' -> { state with Wy = state.Wy - amount }
    | 'S' -> { state with Wy = state.Wy + amount }
    | 'E' -> { state with Wx = state.Wx + amount }
    | 'W' -> { state with Wx = state.Wx - amount }
    | 'L' -> rotateWaypoint state (360 - amount)
    | 'R' -> rotateWaypoint state amount 
    | 'F' -> { state with
                X = state.X + amount * state.Wx
                Y = state.Y + amount * state.Wy }
    | _ -> failwith "invalid action"

let answer2 =
    input
    |> Seq.toList
    |> List.map parseAction
    |> List.fold (fun state (action, amount) -> waypointStep state action amount) { X = 0; Y = 0; Wx = 10; Wy = -1 }
    |> fun s -> printfn "%A" s; manhattan s.X s.Y