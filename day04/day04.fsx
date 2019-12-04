let lower, upper = 372304, 847060

let rec repeat f x n =
    if n = 0 then x else repeat f (f x) (n-1)

let digits' bas number =
    let rec f acc n =
        if n = 0 then
            acc
        else
            f ((n % bas) :: acc) (n / bas)
    f [] number

let digits = digits' 10

let uncurry f (a, b) = f a b

let isAscending =
    List.pairwise >> List.forall (uncurry (<=))
        
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
