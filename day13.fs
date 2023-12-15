module Day13

open IterationTools
open System.IO

type Reflection =
    | Row of int
    | Column of int

let toArray lines = lines |> List.rev |> array2D

let rec parsePatterns acc current lines =
    match current, lines with
    | [], [] -> List.rev acc
    | c, [] -> List.rev ((c |> toArray) :: acc)
    | c, "" :: xs -> parsePatterns ((c |> toArray) :: acc) [] xs
    | c, x :: xs -> parsePatterns acc (x :: c) xs

let difference a b =
    List.zip a b |> Seq.filter (fun (a, b) -> a <> b) |> Seq.length

let rec countErrors acc lines i j =
    if i + j = List.length lines || i - j + 1 < 0 then
        acc
    else
        countErrors (acc + difference lines[i - j + 1] lines[i + j]) lines i (j + 1)

let rec findReflection i errors lines =
    if i = List.length lines - 1 then None
    elif countErrors 0 lines i 1 = errors then Some i
    else findReflection (i + 1) errors lines

let reflection errors pattern =
    let h = pattern |> rows |> findReflection 0 errors
    let v = pattern |> columns |> findReflection 0 errors

    match h, v with
    | Some(r), None -> Row(r)
    | None, Some(c) -> Column(c)
    | None, None -> failwith "no reflection"
    | Some(_), Some(_) -> failwith "two reflections"

let summarize reflection =
    match reflection with
    | Row(r) -> (100) * (r + 1)
    | Column(c) -> c + 1

let part1 patterns =
    patterns |> List.map (reflection 0) |> List.map summarize |> List.sum

let part2 patterns =
    patterns |> List.map (reflection 1) |> List.map summarize |> List.sum

let run =
    printfn "== Day 13 =="

    let patterns =
        File.ReadLines("inputs/day13.txt") |> Seq.toList |> parsePatterns [] []

    printfn "Part 1: %d" (part1 patterns)
    printfn "Part 2: %d" (part2 patterns)
    printfn ""
