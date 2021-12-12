open System.IO

module Array2D =
    let foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
        let mutable state = state
        for x in 0 .. Array2D.length1 array - 1 do
            for y in 0 .. Array2D.length2 array - 1 do
                state <- folder x y state (array.[x, y])
        state

let grid =
    File.ReadLines(@"2021/day11/input.txt")
    |> Seq.map Seq.toArray
    |> Seq.toArray
    |> array2D
    |> Array2D.map (int >> fun s -> s - (int '0'))

type Point = int * int

let neighbors (arr: int [,]) ((x, y): Point): Point array =
    let inBounds (arr: int [,]) (x, y) =
        x >= 0 && y >= 0 && x < Array2D.length1 arr && y < Array2D.length2 arr
    [|
        x - 1, y - 1;
        x - 1, y + 0;
        x - 1, y + 1;
        x + 0, y - 1;
        x + 0, y + 1;
        x + 1, y - 1;
        x + 1, y + 0;
        x + 1, y + 1;
    |]
    |> Array.filter (inBounds arr)

let increment (arr: int[,]) x y =
    let energy = arr.[x, y]
    arr.[x, y] <- energy + 1

let step (arr: int [,]) = 
    Array2D.mapi (fun x y e -> 1 + e) arr

let willFlash x = x > 9
let spent x = x < 0

let flash (arr : int [,]): int [,] =
    let f (x: int) (y: int) (own: int): int =
        let absorbed: int =
            neighbors arr (x, y)
            |> Array.filter (fun (x, y) -> willFlash arr.[x, y])
            |> Array.length
        let energy = own + absorbed
        if willFlash own || spent own then -1 else energy
    Array2D.mapi f arr

let flash' (grid: int [,]): (int * int [,]) =
    let willAnyFlash (arr: int[,]): int =
        Array2D.foldi (fun x y s e -> if willFlash e then 1 + s else s) 0 arr
    let count (arr: int[,]): int =
        Array2D.foldi (fun _ _ s e -> if e < 0 then 1 + s else s) 0 arr
    let reset (arr: int[,]): int [,] =
        Array2D.mapi (fun x y e -> if e < 0 then 0 else e) arr

    let rec flash'' (flashes: int) (grid: int [,]): (int * int [,]) =
        if 0 = willAnyFlash grid then (flashes + count grid, reset grid) else
        flash'' flashes (flash grid)
    flash'' 0 grid

let evolve (stepsLeft: int) (old: int[,]): (int * int [,]) =
    let rec evolve' (stepsLeft: int) (flashesBefore: int) (old: int[,]): (int * int [,]) =
        if stepsLeft = 0 then (flashesBefore, old) else
        let (newflashes, newgrid) = old |> step |> flash'
        printfn "%A \n %A" stepsLeft newgrid
        evolve' (stepsLeft - 1) (flashesBefore + newflashes) newgrid
    evolve' stepsLeft 0 old

grid |> evolve 100 |> printfn "part 1: %A"

let findSync (old: int[,]): int =
    let rec evolve' (steps: int) (old: int[,]): (int * int [,]) =
        let (newFlashes, newGrid) = old |> step |> flash'
        printfn "%A \n %A" (1 + steps) newGrid
        if newFlashes = 100 then (steps, newGrid) else
        evolve' (steps + 1) newGrid
    evolve' 0 old |> fst

grid |> findSync |> printfn "part 2: %A"