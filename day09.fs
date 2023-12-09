module Day09

open System.IO

let parseValues (line: string) =
    line.Split(' ')
    |> Array.map (fun s -> s.Trim())
    |> Array.filter (fun s -> s <> "")
    |> Array.map int
    |> Array.toList

let allZeros values = values |> Seq.forall (fun v -> v = 0)

let rec predictNext values =
    match allZeros values with
    | true -> 0
    | false ->
        let grad = values |> List.pairwise |> List.map (fun (a, b) -> b - a)
        List.last values + predictNext grad

let rec predictPrev values =
    match allZeros values with
    | true -> 0
    | false ->
        let grad = values |> List.pairwise |> List.map (fun (a, b) -> b - a)
        List.head values - predictPrev grad

let part1 sequences =
    sequences |> Array.map predictNext |> Array.sum

let part2 sequences =
    sequences |> Array.map predictPrev |> Array.sum

let run =
    printfn "== Day 09 =="

    let sequences =
        File.ReadAllLines("inputs/day09.txt") |> Array.map parseValues

    printfn "Part 1: %i" (part1 sequences)
    printfn "Part 2: %i" (part2 sequences)
    printfn ""
