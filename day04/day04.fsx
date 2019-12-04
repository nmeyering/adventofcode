let lower, upper = 372304, 847060

let digits number: int list =
    [
        number / 100000 % 10;
        number / 10000 % 10;
        number / 1000 % 10;
        number / 100 % 10;
        number / 10 % 10;
        number % 10
    ]

let isAscending (number: int list) =
    match number with
    | [] -> failwith "asdasd"
    | _ :: rest -> 
        List.fold2 (fun s a b -> s && (a <= b)) true number (rest @ [9])
        
let hasRepeated (number: int list) =
    let ds = number |> List.toArray
    ds.[0] = ds.[1] ||
    ds.[1] = ds.[2] ||
    ds.[2] = ds.[3] ||
    ds.[3] = ds.[4] ||
    ds.[4] = ds.[5]

let criteria number =
    let ds = digits number
    isAscending ds && hasRepeated ds

let answer1 = List.filter criteria [lower..upper] |> List.length

let streaks (xs: int list): int list =
    let folder (streaks, lastDigit) digit =
        match streaks with
        | [] -> ([1], digit)
        | s  :: rest ->
             if digit = lastDigit then
                (s+1 :: rest, digit)
             else
                (1 :: s :: rest, digit)
    List.fold folder ([], 0) xs |> fst

let (&&>) p1 p2 x =
    p1 x && p2 x

let criteria2 = criteria &&> (digits >> streaks >> List.contains 2)
let answer2 = List.filter criteria2 [lower..upper] |> List.length
