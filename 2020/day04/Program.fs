open System
open System.IO

let splitOn f =
    let grouper (acc : string list list) (x: string): (string list list) =
        match acc with
        | [] -> [[x]]
        | h :: t -> if f x then ([] :: h :: t) else ((x :: h) :: t)
    List.fold grouper [] >> List.rev

type PassportField =
    { Key: string
      Rule: string -> bool }

type Height = In of int | Cm of int

let tryParseInt (s: string) =
    Int32.TryParse s |> function | (true, parsed) -> Some parsed | _ -> None

let (|Unit|_|) (u:string) (s:string) =
    if s.EndsWith u then
        let amount = (s.Remove (s.Length - u.Length)) in
        match u with
        | "cm" -> tryParseInt amount |> Option.map Cm
        | "in" -> tryParseInt amount |> Option.map In
        | _ -> None
    else
        None

let parseHeight = function
               | Unit "cm" (Cm cm) -> Some (Cm cm)
               | Unit "in" (In in') -> Some (In in')
               | _ -> None

let isHexDigit (c: char): bool =
    Char.IsDigit c || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F'

let hairColor (input: string) =
    input.Length = 7 && input.Substring 1 |> Seq.forall isHexDigit

let eyeColor input =
    List.contains input ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]

let passportId (input: string) =
    input.Length = 9 && input |> Seq.forall (Char.IsDigit)

let expectedFields = [
    ( "byr", fun byr -> let year = int byr in year >= 1920 && year <= 2002 );
    ( "iyr", fun iyr -> let year = int iyr in year >= 2010 && year <= 2020 );
    ( "eyr", fun eyr -> let year = int eyr in year >= 2020 && year <= 2030 );
    ( "hgt", function
               | Unit "cm" (Cm cm) -> cm >= 150 && cm <= 193
               | Unit "in" (In in') -> in' >= 59 && in' <= 76 
               | _ -> false );
    ( "hcl", hairColor );
    ( "ecl", eyeColor );
    ( "pid", passportId );] |> Map.ofList

let split (pattern: string) (xs: string) =
    xs.Split pattern |> Seq.toList

let allFieldsPresent (fields: string list): bool =
    let keys = List.map ((split ":") >> (List.head)) fields
    expectedFields |> Map.toSeq |> Seq.map fst |>
        Seq.forall (fun f -> List.contains f keys)

let fieldsValid passport =
    let fields = passport |> List.map ((split ":") >> (function | [k; v] -> Some (k, v) | _ -> None))
    let validate (k, v) = match Map.tryFind k expectedFields with
                          | Some f -> if (not <| f v) then printfn "%s wrong" k; f v else f v
                          | None -> false
    fields |> Seq.forall (function | Some ("cid", _) -> true | Some field -> validate field | None -> false )

let lines = File.ReadAllLines(@"day04/input.txt") |> Array.toList

let passports =
    lines
    |> splitOn (fun x -> x = "")
    |> List.map (fun x -> String.Join (" ", x))
    |> List.map ((split " ") >> Seq.toList)

let answer1 =
    passports
    |> List.filter allFieldsPresent
    |> List.length

let answer2 =
    passports
    |> List.filter (fun x -> allFieldsPresent x && fieldsValid x)
    |> List.length

printfn "answer part 1: %A" answer1
printfn "answer part 2: %A" answer2