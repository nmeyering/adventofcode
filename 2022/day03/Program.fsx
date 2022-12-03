open System
open System.IO

let inputs =
    File.ReadLines(@"day03/input.txt")
    |> Seq.toList

// let inputs = [
//     "vJrwpWtwJgWrhcsFMMfFFhFp"
//     "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
//     "PmmdzqPrVvPwwTWBwg"
//     "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
//     "ttgJtRGJQctTZtZT"
//     "CrZsJsPPZsGzwwsLwLmpwMDw"
// ]

let compartments (line: string) =
    let len  = String.length line
    let cs = line.ToCharArray()
    let a = cs |> Array.take (len / 2)
    let b = cs |> Array.skip (len / 2)
    a, b

let priority (c: char): int =
    match c with
    | c when c >= 'A' && c <= 'Z' -> 27 + int c - (int 'A')
    | c when c >= 'a' && c <= 'z' -> 1 + int c - (int 'a')
    | _ -> failwith "invalid letter"

let commonElement (left: char array) (right: char array): char option =
    match Set.intersect (Set(left)) (Set(right)) |> Set.toList with
    | [x] -> Some x
    | _ -> None

let answer1 =
    inputs
    |> List.map (
        compartments
        >> fun (x, y) -> commonElement x y
        >> Option.get
        >> priority
        )
    |> List.sum

printfn "Part 1: %A" answer1

let commonElement3 (a: char array) (b: char array) (c: char array): char option =
    let ab = Set.intersect (set a) (set b)
    let abc = Set.intersect ab (set c)
    match abc |> Set.toList with
    | [x] -> Some x
    | _ -> None

let answer2 =
    inputs
    |> List.map (fun x -> x.ToCharArray ())
    |> List.chunkBySize 3
    |> List.map (
         function [x;y;z] -> commonElement3 x y z | _ -> None
        >> Option.get
    )
    |> List.sumBy priority

printfn "Part 2: %A" answer2