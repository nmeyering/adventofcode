open System
open System.IO

let inputs =
    File.ReadLines(@"day05/input.txt")
    |> Seq.toList

// let inputs = [
//     "    [D]    "
//     "[N] [C]    "
//     "[Z] [M] [P]"
//     " 1   2   3 "
//     ""
//     "move 1 from 2 to 1"
//     "move 3 from 1 to 3"
//     "move 2 from 2 to 1"
//     "move 1 from 1 to 2"
// ]

let stacks inputs =
    inputs
    |> List.takeWhile (fun x -> x <> "")
    |> List.map (fun x -> x.ToCharArray ())
    |> List.toArray
    |> Array.transpose
    |> Array.map (
        Array.rev
        >> Array.skip 1
        >> Array.takeWhile (fun c -> c <> ' ')
        >> Array.rev
        >> Array.toList
    )
    |> Array.indexed
    |> Array.filter (fun (i, _) -> i % 4 = 1)
    |> Array.map snd

let moveOne (stacks: char list array) (srcIndex: int) (dstIndex: int) =
    let item :: rest = stacks[srcIndex - 1]
    let dst = stacks[dstIndex - 1]
    Array.set stacks (srcIndex - 1) rest
    Array.set stacks (dstIndex - 1) (item :: dst)
    stacks
    
let rec move (stacks: char list array) (amount: int) (srcIndex: int) (dstIndex: int) =
    if amount = 0
    then stacks
    else move (moveOne stacks srcIndex dstIndex) (amount - 1) srcIndex dstIndex

let message (stacks: char list array) =
    stacks |> Array.map List.head |> fun x -> new String (x)
    
let moves =
    inputs
    |> List.skipWhile (fun x -> x <> "")
    |> List.skip 1
    |> List.map (
         fun line -> line.Split ()
         >> Array.indexed
         >> Array.filter (fun (i, _) -> i % 2 = 1)
         >> Array.map (fun (_, s) -> int s)
       )
    
let applyMove move (stack: char list array) (m: int array): char list array =
    move stack m[0] m[1] m[2]

let answer1 = 
    moves
    |> List.fold (applyMove move) (stacks inputs)
    |> message
    
printfn "Part 1: %s" answer1

let moveMultiple (stacks: char list array) (amount: int) (srcIndex: int) (dstIndex: int) =
    let src = stacks[srcIndex - 1]
    let item, rest = List.splitAt amount src
    let dst = stacks[dstIndex - 1]
    let newDst = (List.append item dst)
    Array.set stacks (srcIndex - 1) rest
    Array.set stacks (dstIndex - 1) newDst
    stacks

let answer2 = 
    moves
    |> List.fold (applyMove moveMultiple) (stacks inputs)
    |> message
    
printfn "Part 2: %s" answer2