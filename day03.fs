module Day03

open System
open System.IO

type Cell =
    | Empty
    | Symbol of int * char
    | Digit of char

let parseLine row (line: string) =
    line
    |> Seq.mapi (fun col c ->
        match c with
        | c when Char.IsDigit(c) -> Digit(c)
        | '.' -> Empty
        | _ -> Symbol(row * String.length line + col, c))
    |> Seq.toList

let parse (lines: string list) =
    lines |> Seq.mapi parseLine |> Seq.toList

let isValid schematic row column =
    row >= 0
    && row < List.length schematic
    && column >= 0
    && column < List.length schematic.[row]

let adjacentSymbols schematic row column =
    let adjacentCells =
        seq {
            for r in [ row - 1 .. row + 1 ] do
                for c in [ column - 1 .. column + 1 ] do
                    if isValid schematic r c then
                        yield schematic.[r].[c]
        }

    adjacentCells
    |> Seq.map (function
        | Symbol(i, s) -> Some((i, s))
        | _ -> None)
    |> Seq.choose id
    |> Seq.toList

let rec findPartNumbers row column current adjacent acc schematic =
    if row = List.length schematic then
        List.rev acc
    elif column = List.length schematic.[row] then
        findPartNumbers (row + 1) 0 current adjacent acc schematic
    else
        match schematic.[row].[column] with
        | Digit(c) ->
            let newAdjacent = adjacent @ adjacentSymbols schematic row column
            findPartNumbers row (column + 1) (current + string (c)) newAdjacent acc schematic
        | _ ->
            if adjacent.IsEmpty then
                findPartNumbers row (column + 1) "" [] acc schematic
            else
                findPartNumbers row (column + 1) "" [] ((int current, adjacent |> List.distinct) :: acc) schematic

let part1 partNumbers = partNumbers |> List.sumBy fst

let part2 partNumbers =
    partNumbers
    |> Seq.map (fun (n, adjacent) ->
        adjacent
        |> Seq.map (function
            | (i, '*') -> Some(i, n)
            | _ -> None))
    |> Seq.concat
    |> Seq.choose id
    |> Seq.toList
    |> List.groupBy fst
    |> List.filter (fun (_, g) -> List.length g = 2)
    |> List.map (fun (_, g) -> g |> List.fold (fun acc (_, n) -> acc * n) 1)
    |> List.sum

let run =
    printfn "== Day 03 =="

    let lines = File.ReadLines("inputs/day03.txt") |> Seq.toList
    let partNumbers = lines |> parse |> findPartNumbers 0 0 "" [] []

    printfn "Part 1: %A" (part1 partNumbers)
    printfn "Part 2: %A" (part2 partNumbers)
    printfn ""
