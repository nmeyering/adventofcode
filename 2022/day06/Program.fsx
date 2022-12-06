open System
open System.IO

let inputs =
    File.ReadLines(@"day06/input.txt")
    |> Seq.toList

// let inputs = [
//     "bvwbjplbgvbhsrlpgdmjqwftvncz"
//     "nppdvjthqldpwncqszvftbrmjlhg"
//     "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
//     "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
// ]

let allDifferent (xs: 'a list): bool =
    let uniques = xs |> set |> Set.count
    List.length xs = uniques

let find' (length: int) (chars: char array): int option =
    chars |> Array.toList |> List.windowed length
    |> List.tryFindIndex allDifferent
    |> Option.map ((+) length)

let findStartOfPacket (chars: char array): int option =
    find' 4 chars

let findStartOfMessage (chars: char array): int option =
    find' 14 chars

let answer1 =
    inputs
    |> List.map (
        fun line -> line.ToCharArray ()
        >> findStartOfPacket
    )
    
printfn "Part 1: %A" answer1
    
let answer2 =
    inputs
    |> List.map (
        fun line -> line.ToCharArray ()
        >> findStartOfMessage
    )
    
printfn "Part 2: %A" answer2