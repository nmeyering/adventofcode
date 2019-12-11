type OrbitMap = Map<string, string>
type Orbit = string * string

let buildMap (pairs: Orbit list): OrbitMap =
    let flip = List.map (fun (x, y) -> (y, x))
    pairs |> flip |> Map.ofList


let rec orbits (directOrbits : OrbitMap) (x : string) : string list =
    match Map.tryFind x directOrbits with
    | Some predecessor -> predecessor :: (orbits directOrbits predecessor)
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

let folder (s: int) (k: string) _ =
    let orbitCount = k |> orbits orbitMap |> List.length
    orbitCount + s
    
let planets = Map.fold folder 0 orbitMap
