open System.IO

module Array2D =
    let foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
        let mutable state = state
        for x in 0 .. Array2D.length1 array - 1 do
            for y in 0 .. Array2D.length2 array - 1 do
                state <- folder x y state (array.[x, y])
        state

type Point = int * int

let neighbors (arr: int [,]) ((x, y): Point): Point array =
    let inBounds (arr: int [,]) (x, y) =
        x >= 0 && y >= 0 && x < Array2D.length1 arr && y < Array2D.length2 arr
    [|
        x + 0, y - 1;
        x - 1, y + 0;
        x + 1, y + 0;
        x + 0, y + 1;
    |]
    |> Array.filter (inBounds arr)

let neighborValues (arr: int [,]) (p: Point): int array =
    neighbors arr p
    |> Array.map (fun (x, y) -> Array2D.get arr x y)

let all (pred: 'a -> bool) =
    Seq.fold (fun state i -> state && (pred i)) true

let lowPoint (arr: int [,]) (p: Point) (here: int): bool =
    neighborValues arr p
    |> all (fun x -> x > here)

let grid =
    File.ReadLines(@"day09/input.txt")
    |> Seq.toArray
    |> Seq.map Seq.toArray
    |> array2D
    |> Array2D.map (int >> fun s -> s - (int '0'))

let risk (x, y) = 1 + Array2D.get grid x y

let lowPoints =
    grid |> Array2D.foldi (fun x y state t ->
        match lowPoint grid (x, y) t with
        | true -> (x, y) :: state
        | false -> state) []

lowPoints
    |> Seq.sumBy risk
    |> printfn "part 1: %A"

let rec basinSize' (arr : int [,]) (size: int) (x : int) (y : int): int =
    if (arr.[x, y] <> 0) then 0 else
    arr.[x, y] <- 1
    let ns = neighbors arr (x, y) |> Array.filter (fun (x, y) -> Array2D.get arr x y |> (fun p -> p = 0))
    1 + match ns with
        | [||] -> 0
        | _ -> ns |> Seq.sumBy (fun (x, y) ->
            basinSize' arr size x y)

let basinSize (arr : int [,]) (x : int) (y : int): int =
    let arr' = arr |> Array2D.map (fun p -> if p < 9 then 0 else 9)
    basinSize' arr' 0 x y

lowPoints
    |> Seq.map (fun (x, y) -> basinSize grid x y) |> Seq.toList
    |> List.sortDescending
    |> List.take 3
    |> List.reduce (*)
    |> printfn "part 2: %A"