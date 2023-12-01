module Day01

open System
open System.IO

let digitText = [
    ("one", "o1e"); 
    ("two", "t2o"); 
    ("three", "t3e");
    ("four", "4");
    ("five", "5e");
    ("six", "6");
    ("seven", "7n");
    ("eight", "e8t");
    ("nine", "n9e")
]

let to_digits (s:string) =
    s |> Seq.map (fun c -> 
        match Char.IsDigit(c) with
        | true -> Some (int c - int '0')
        | _ -> None
    ) |> Seq.choose id |> Seq.toList

let calibration_value digits =
    List.head digits * 10 + List.last digits

let rec replaceText i (s: string) =
    if i = digitText.Length then s
    else
        let (text, replacement) = digitText.[i]
        let s = s.Replace(text, replacement)
        replaceText (i + 1) s


let part1 (values : string seq) =
    values
    |> Seq.map to_digits
    |> Seq.map calibration_value
    |> Seq.sum 


let part2 (values : string list) =
    values 
    |> Seq.map (replaceText 0)
    |> part1

let run =
    printfn "== Day 01 =="
    
    let values = 
        File.ReadLines("inputs/day01.txt")
        |> Seq.toList
    
    printfn "Part 1: %i" (part1 values)
    printfn "Part 2: %i" (part2 values)
    printfn ""
