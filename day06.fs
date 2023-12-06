module Day06

open System.IO

type Record = { Time: int64; Distance: int64 }

let parseLine (line: string) =
    line.Substring(line.IndexOf(':') + 1).Split(' ')
    |> Array.map (fun s -> s.Trim())
    |> Array.filter (fun s -> s <> "")
    |> Array.toList

let parseRecords (lines: string array) =
    let times = parseLine lines.[0]
    let distances = parseLine lines.[1]

    List.zip times distances
    |> List.map (fun (t, d) -> { Time = int64 t; Distance = int64 d })

let parseFixedRecord (lines: string array) =
    let times = parseLine lines.[0]
    let distances = parseLine lines.[1]

    { Time = int64 (String.concat "" times)
      Distance = int64 (String.concat "" distances) }

let isWin record x = (record.Time - x) * x > record.Distance

let rec findStart record x =
    if isWin record x then findStart record (x - 1L) else x + 1L

let rec findEnd record x =
    if isWin record x then findEnd record (x + 1L) else x

let countWins record =
    let a = -1.
    let b = double record.Time
    let c = double -record.Distance
    let test = b * b - 4. * a * c

    if test <= 0. then
        0L
    else
        let x0 = (-b + sqrt (test)) / (2. * a)
        let x1 = (-b - sqrt (test)) / (2. * a)
        let s = findStart record (int64 x0)
        let e = findEnd record (int64 x1)
        e - s

let part1 lines =
    lines |> parseRecords |> List.map countWins |> List.fold (*) 1L

let part2 lines = lines |> parseFixedRecord |> countWins

let run =
    printfn "== Day 06 =="

    let lines = File.ReadAllLines("inputs/day06.txt")

    printfn "Part 1: %i" (part1 lines)
    printfn "Part 2: %i" (part2 lines)
    printfn ""
