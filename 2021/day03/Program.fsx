open System
open System.IO

let lines = File.ReadLines(@"day03/input.txt")
//let lines = [
//    "00100"
//    "11110"
//    "10110"
//    "10111"
//    "10101"
//    "01111"
//    "00111"
//    "11100"
//    "10000"
//    "11001"
//    "00010"
//    "01010"
//]

let acc = lines |> Seq.head |> Seq.map (fun _ -> 0) |> Seq.toArray

type Bit = Zero | One

let toBit (c: char): Bit = if c = '0' then Zero else One

let parseBinary (line: string): Bit array =
     Seq.toArray line |> Array.map toBit

let gobble (acc: int array) (line: string): int array =
    acc
    |> Array.zip (parseBinary line)
    |> Array.map (fun (bit, number) ->
        match bit with
        | Zero -> number
        | One -> number + 1
    )

let len = lines |> Seq.length

let toAnswer len (sums: int array): Bit array =
    sums
    |> Array.map (fun x -> if x > len / 2 then One else Zero)

let epsilonRate =
    lines
    |> Seq.fold gobble acc
    |> toAnswer len

let bitwiseNot (b: Bit): Bit =
    match b with
    | One -> Zero
    | Zero -> One

let gammaRate = epsilonRate |> Array.map bitwiseNot

let toDecimal (bits: Bit array) =
    let folder =
        fun bit (sum, exp) ->
            match bit with
            | One -> sum + exp, 2 * exp
            | Zero -> sum, 2 * exp
    Array.foldBack folder bits (0, 1)
    |> fst

let answer1 =
    (epsilonRate |> toDecimal) * (gammaRate |> toDecimal)

printfn $"answer part 1: %A{answer1}"

let toInt (bit: Bit): int =
    match bit with
    | Zero -> 0
    | One -> 1

let numbers = lines |> Seq.map parseBinary

let mostCommon (bits: Bit list): Bit =
    let len = bits |> List.length
    let ones = bits |> List.sumBy toInt
    let zeros = len - ones
    match ones - zeros with
    | x when x < 0 -> Zero
    | _ -> One

let mostCommonAt (n: int) (bitss: Bit array seq) =
    bitss
    |> Seq.map (fun x -> x.[n])
    |> Seq.toList
    |> mostCommon

let rec f' (invert: bool) (n: int) (numbers: Bit array seq) =
    let m = mostCommonAt n numbers |> (fun x -> if invert then bitwiseNot x else x)
    let pass = numbers |> Seq.where (fun line -> line.[n] = m) |> Seq.toList
    match pass with
    | [] -> failwith ":("
    | [x] -> x
    | xs -> f' invert (n + 1) xs

let f invert numbers = f' invert 0 numbers

let o2rating = f false numbers |> toDecimal
let co2rating = f true numbers |> toDecimal

let answer2 = o2rating * co2rating
printfn $"answer part 2: %A{answer2}"
