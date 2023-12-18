module Day18

open System.IO

type Point = { X: int64; Y: int64 }

type Edge = { Start: Point; End: Point }

let compareEdges e0 e1 =
    let c0 = compare e0.Start.X e1.Start.X
    if c0 <> 0 then c0 else compare e0.End.X e1.End.X

let edge p0 p1 =
    if p0.Y < p1.Y then { Start = p0; End = p1 }
    elif p0.X < p1.X then { Start = p0; End = p1 }
    else { Start = p1; End = p0 }

let rec parsePlan1 edges p0 (lines: string list) =
    if List.length lines = 0 then
        edges |> List.sortWith compareEdges
    else
        let line = List.head lines
        let parts = line.Split()
        let d = int64 parts[1]

        let p1 =
            match parts[0] with
            | "U" -> { p0 with Y = p0.Y - d }
            | "D" -> { p0 with Y = p0.Y + d }
            | "L" -> { p0 with X = p0.X - d }
            | "R" -> { p0 with X = p0.X + d }
            | _ -> failwith "Invalid direction"

        parsePlan1 ((edge p0 p1) :: edges) p1 (List.tail lines)

let rec parsePlan2 edges p0 (lines: string list) =
    if List.length lines = 0 then
        edges |> List.sortWith compareEdges
    else
        let line = List.head lines
        let parts = line.Split()
        let hex = parts[2][2..7]
        let d = int64 ("0x" + hex[..4])

        let p1 =
            match hex[5] with
            | '0' -> { p0 with X = p0.X + d }
            | '1' -> { p0 with Y = p0.Y + d }
            | '2' -> { p0 with X = p0.X - d }
            | '3' -> { p0 with Y = p0.Y - d }
            | _ -> failwith "invalid direction"

        parsePlan2 ((edge p0 p1) :: edges) p1 (List.tail lines)

let intersects y e = (e.Start.Y < y && e.End.Y >= y)

let touches y e = (e.Start.Y <= y && e.End.Y >= y)

let compareSpans (a0, b0) (a1, b1) =
    let i0 = compare a0 a1
    if i0 <> 0 then i0 else compare b0 b1

let rec mergeSpans merged spans =
    match spans with
    | [] -> List.rev merged
    | (a0, b0) :: (a1, b1) :: xs when a1 <= b0 -> mergeSpans merged ((a0, max b0 b1) :: xs)
    | x :: xs -> mergeSpans (x :: merged) xs

let xRange e = e.Start.X, e.End.X

let countInside plan y =
    plan
    |> List.filter (intersects y)
    |> List.map (fun e -> e.Start.X)
    |> List.chunkBySize 2
    |> List.choose (function
        | [ x; y ] -> Some(x, y)
        | _ -> None)
    |> List.append (plan |> List.filter (touches y) |> List.map xRange)
    |> List.sortWith compareSpans
    |> mergeSpans []
    |> List.fold (fun acc (a, b) -> acc + b - a + 1L) 0L

let isHorizontal e = e.Start.Y = e.End.Y

let yRanges plan =
    plan
    |> List.choose (fun e -> if isHorizontal e then Some(e.Start.Y) else None)
    |> List.sort
    |> List.pairwise
    |> List.filter (fun (y0, y1) -> y1 > y0)
    |> List.collect (fun (y0, y1) -> [ y0, 1L; y0 + 1L, y1 - y0 - 1L; y1, 1L ])
    |> List.distinct

let measureLagoon plan =
    yRanges plan |> List.fold (fun acc (y, h) -> acc + (countInside plan y) * h) 0L

let part1 lines =
    parsePlan1 [] { X = 0L; Y = 0L } lines |> measureLagoon

let part2 lines =
    parsePlan2 [] { X = 0L; Y = 0L } lines |> measureLagoon

let run =
    printfn "== Day 18 =="

    let lines = File.ReadLines("inputs/day18.txt") |> Seq.toList
    printfn "Part 1: %d" (part1 lines)
    printfn "Part 2: %d" (part2 lines)
    printfn ""
