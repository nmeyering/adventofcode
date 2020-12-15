open System.IO
open System

let input = [18;8;0;5;4;1;20]
// let input = [0;3;6]

let game (maxTurn: int) (input: int list) =
    let initMap (xs: int list): Map<int, (int * int)> =
        xs
            |> List.indexed
            |> List.map (fun (i, x) -> (x, (1 + i, 0))) // turns start at 1
            |> Map.ofList 

    let markTurn turn spoken map =
        match Map.tryFind spoken map with
        | None -> Map.add spoken (turn, 0) map
        | Some (prev, _) -> Map.add spoken (turn, prev) map

    let rec game' (turn: int) (lastSpoken: int) (map: Map<int, (int * int)>) =
        if turn = maxTurn then lastSpoken else
        match Map.tryFind lastSpoken map with
        | None -> game' (turn + 1) 0 (markTurn turn 0 map)
        | Some (_, 0) -> let spoken = 0 in game' (turn + 1) spoken (markTurn turn spoken map) 
        | Some (previousTurn, previousPreviousTurn) -> let spoken = previousTurn - previousPreviousTurn in game' (turn + 1) spoken (markTurn turn spoken map)

    game' (1 + List.length input) (List.last input) (initMap input)