type OrbitMap = Map<string, string>
type Orbit = string * string

let buildMap (pairs: Orbit list): OrbitMap =
    let flip = List.map (fun (x, y) -> (y, x))
    pairs |> flip |> Map.ofList


let rec orbits' (directOrbits : OrbitMap) (x : string) : string list =
    match Map.tryFind x directOrbits with
    | Some predecessor -> predecessor :: (orbits' directOrbits predecessor)
    | None -> []

// let input = [
//     ("COM", "B")
//     ("B", "C")
//     ("C", "D")
//     ("D", "E")
//     ("E", "F")
//     ("B", "G")
//     ("G", "H")
//     ("D", "I")
//     ("E", "J")
//     ("J", "K")
//     ("K", "L")
// ]
open System.IO

let input = 
    File.ReadLines(@"day06/input.txt")
    |> Seq.map (fun line ->
                     line.Split ')'
                     |> fun parts ->
                        match parts with
                        | [|a;b|] -> (a,b)
                        | _        -> failwith "syntax error"
               )
    |> Seq.toList            

let orbitMap = input |> buildMap

let orbits = orbits' orbitMap

let folder (s: int) (k: string) _ =
    let orbitCount = k |> orbits |> List.length
    orbitCount + s

let myOrbits = orbits "YOU" |> List.rev
let santaOrbits = orbits "SAN" |> List.rev

let rec requiredTransfers orbits1 orbits2 =
    match (orbits1, orbits2) with
    | [], r -> List.length r
    | l, [] -> List.length l
    | (orbit1 :: rest1), (orbit2 :: rest2) ->
        if orbit1 = orbit2 then requiredTransfers rest1 rest2 else (List.length orbits1) + (List.length orbits2)

let answer1 = Map.fold folder 0 orbitMap
let answer2 = requiredTransfers myOrbits santaOrbits
