open System.IO

let crabs =
    File.ReadLines(@"2021/day07/input.txt")
    |> Seq.head
    |> fun s -> s.Split(',')
    |> Seq.map int
    |> Seq.toList

let fuelCost1 target x = abs (x - target)

let totalFuel (fuelCost: int -> int -> int) (xs: int list) (target: int): int =
    List.sumBy (fuelCost target) xs

let target, fuel =
    [ List.min crabs .. List.max crabs ]
    |> List.map (fun crab -> let fuel = totalFuel fuelCost1 crabs crab in (crab, fuel))
    |> List.minBy snd

printfn "part 1: %A" fuel

let fuelCost2 target x =
    let f = fuelCost1 target x
    (f * (f + 1)) / 2

let answer2 =
    [ List.min crabs .. List.max crabs ]
    |> List.minBy (totalFuel fuelCost2 crabs)

printfn "part 2: %A" answer2