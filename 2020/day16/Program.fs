open System.IO
open System

let splitOn f =
    let grouper (acc : string list list) (x: string): (string list list) =
        match acc with
        | [] -> [[x]]
        | h :: t -> if f x then ([] :: h :: t) else ((x :: h) :: t)
    List.rev << List.map List.rev << List.fold grouper []

type Rule = string * (int * int) * (int * int)

let parseRule (line: string): Rule =
    printfn "rule: %A" line
    let [|key; rest|] = line.Split (": ")
    let [|r1; r2|] = rest.Split (" or ")
    let [|minR1; maxR1|] = r1.Split ("-")
    let [|minR2; maxR2|] = r2.Split ("-")
    (key, (int minR1, int maxR1), (int minR2, int maxR2))

let parseTicket (line: string): int list =
    line.Split (",") |> Seq.map int |> Seq.toList

let isBetween min max value =
    min <= value && value <= max

let errorRate (rules: Rule list) ticket =
    ticket 
    |> List.sumBy (fun value ->
        rules 
        |> List.map (fun (_, (min1, max1), (min2, max2)) -> isBetween min1 max1 value || isBetween min2 max2 value)
        |> List.reduce (||)
        |> fun validForAnyField -> if validForAnyField then 0 else value
    )


let findBadTickets rules tickets =
    tickets
    |> List.map (fun ticket -> errorRate rules ticket)

let input = File.ReadAllLines ("day16/input.txt") |> Seq.toList

let answer1 =
    input
    |> splitOn ((=)"")
    |> function
        | [rulesSection; _; ticketsSection] -> 
            let rules = rulesSection |> List.map parseRule
            let tickets = ticketsSection |> List.skip 1 |> List.map parseTicket
            Some (findBadTickets rules tickets)
        | _ -> None
    |> Option.map List.sum

let determineFields (rules: Rule list) (myTicket: int list) (tickets: int list list) =
    None

let answer2 =
    input
    |> splitOn ((=)"")
    |> function
        | [rulesSection; myTicketSection; ticketsSection] -> 
            let rules = rulesSection |> List.map parseRule
            let tickets = ticketsSection |> List.skip 1 |> List.map parseTicket
            let goodTickets = tickets |> List.filter (errorRate rules >> (>)0)
            let myTicket = myTicketSection |> function | [t] -> parseTicket t | _ -> failwith "my ticket section contains more than one line"
            determineFields rules myTicket goodTickets
        | _ -> None