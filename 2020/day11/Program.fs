open System.IO

let board = File.ReadAllLines ("day11/input.txt") |> array2D

let directions =
    [
        (-1, +0)
        (+1, +0)
        (-1, -1)
        (+0, -1)
        (+1, -1)
        (-1, +1)
        (+0, +1)
        (+1, +1)
    ]

let neighbors x y =
    directions
    |> List.map (fun (d1, d2) -> (x + d1, y + d2))

let tryGet (board: char[,]) x y =
    if x < Array2D.length1 board && x >= 0 && y < Array2D.length2 board && y >= 0
        then Some (Array2D.get board x y)
        else None

let occupiedNeighbors board x y: int =
    neighbors x y
    |> List.map (fun (i, j) -> tryGet board i j)
    |> List.filter ((=)(Some '#'))
    |> List.length

let step neighborFunc maxNeighbors (board: char[,]): char[,] =
    let step' x y c =
        match (c, neighborFunc board x y) with 
            | 'L', 0 -> '#'
            | '#', n when n >= maxNeighbors -> 'L'
            | _ -> c
    Array2D.mapi step' board


let rec evolve neighborFunc maxNeighbors board =
    let board' = step neighborFunc maxNeighbors board
    if board = board'
        then board'
        else evolve neighborFunc maxNeighbors board'

let endstate1 =
    board
    |> evolve occupiedNeighbors 4

let countOccupied board =
    board
    |> fun board -> seq { for x in [0..(Array2D.length1 board) - 1] do 
              for y in [0..(Array2D.length2 board) - 1] do 
                  yield board.[x, y] }
    |> Seq.countBy id
    |> Map.ofSeq
    |> Map.tryFind '#'

let answer1 =
    endstate1
    |> countOccupied

let watch board (x, y) (dirX, dirY): bool =
    let rec watch' x y =
        match tryGet board x y with
        | Some '#' -> true
        | Some '.' -> watch' (x + dirX) (y + dirY)
        | _ -> false
    watch' (x + dirX) (y + dirY)

let occupiedVisibleNeighbors board x y =
    directions
    |> List.map (fun d -> watch board (x, y) d)
    |> List.filter ((=)true)
    |> List.length

let answer2 =
    board
    |> evolve occupiedVisibleNeighbors 5
    |> countOccupied