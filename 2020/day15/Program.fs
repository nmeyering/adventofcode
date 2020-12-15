open System.IO
open System

// let input = [18;8;0;5;4;1;20]
let input = [0;3;6]

let game (maxTurn: int) (input: int list) =
    let initMap (xs: int list): Map<int, (int * int)> =
        xs
            |> List.indexed
            |> List.map (fun (i, x) -> (x, (1 + i, 0))) // turns start at 1
            |> Map.ofList 

    let game' (turn: int, lastSpoken: int, map: Map<int, (int * int)>): (int * (int * int * Map<int, (int * int)>)) option =
        if turn % 100000 = 0 then printfn "turn %A, last spoken: %A, %A" turn lastSpoken map else ()
        let markTurn turn spoken map =
            match Map.tryFind spoken map with
            | None -> Map.add spoken (turn, 0) map
            | Some (prev, _) -> Map.add spoken (turn, prev) map

        if turn = maxTurn then None else
        match Map.tryFind lastSpoken map with
        | None -> Some (0, (turn + 1, 0, markTurn turn 0 map))
        | Some (_, 0) -> let spoken = 0 in Some (spoken, (turn + 1, spoken, markTurn turn spoken map)) 
        | Some (previousTurn, previousPreviousTurn) -> let spoken = previousTurn - previousPreviousTurn in Some (spoken, (turn + 1, spoken, markTurn turn spoken map))

    List.unfold game' (1 + List.length input, List.last input, initMap input)
    |> List.last 

input
|> game 30000001