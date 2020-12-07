open System
open System.IO

let lines = File.ReadAllLines(@"day03/input.txt") |> Array.toList

let map = array2D lines

let positions (map: char [,]) (right: int) (down: int): (int * int) list =
    let width = Array2D.length2 map
    let height = Array2D.length1 map
    List.unfold (fun (x, y) -> 
        if x >= height then None else Some ((x, y), (x + down, (y + right) % width))) (0, 0)

let tile (map: 'a [,]) (x, y) =
    map.[x, y]

let slopes = [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]

let answers = slopes |> List.map (fun (right, down) -> positions map right down) |> List.map (fun route ->
    route
    |> List.map (tile map)
    |> List.filter (fun x -> x = '#')
    |> List.length
)

let answer = answers |> List.map int64 |> List.reduce (*)