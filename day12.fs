module Day12

open System.Collections.Generic
open System.IO

type Row = { Springs: string; Sizes: string }

type Size =
    { Head: int
      Tail: string }

    static member parse(sizes: string) =
        match sizes.IndexOf(',') with
        | -1 -> { Head = int sizes; Tail = "" }
        | i ->
            { Head = int sizes[.. i - 1]
              Tail = sizes[i + 1 ..] }

    static member pop size = Size.parse size.Tail

    static member next size = { size with Head = size.Head - 1 }

    static member isEmpty size = size.Head = 0 && size.Tail = ""

type Args = string * Size * bool

type Cache = Dictionary<Args, uint64>

let parseLine (line: string) =
    let parts = line.Split(' ')

    { Springs = parts.[0]
      Sizes = parts.[1] }

let rec countArrangements (cache: Cache) args =
    if cache.ContainsKey(args) then
        cache.[args]
    else
        let springs, size, inside = args

        let v =
            if String.length springs = 0 then
                if Size.isEmpty size then 1UL else 0UL
            else
                match size, springs[0], inside with
                | { Head = 0; Tail = "" }, '#', _ -> 0UL
                | { Head = 0; Tail = "" }, _, _ -> countArrangements cache (springs[1..], size, false)
                | { Head = 0 }, ',', true -> countArrangements cache (springs[1..], (Size.pop size), false)
                | { Head = 0 }, '?', true -> countArrangements cache (springs[1..], (Size.pop size), false)
                | { Head = 0 }, '#', true -> 0UL
                | _, ',', true -> 0UL
                | _, _, true -> countArrangements cache (springs[1..], (Size.next size), true)
                | _, ',', false -> countArrangements cache (springs[1..], size, false)
                | _, '?', false ->
                    (countArrangements cache (springs[1..], size, false))
                    + (countArrangements cache (springs[1..], (Size.next size), true))
                | _, '#', false -> countArrangements cache (springs[1..], (Size.next size), true)
                | _ -> failwith "Invalid state"

        cache.[args] <- v
        v

let countRowArrangements row =
    let cache = new Dictionary<Args, uint64>()

    let springs =
        row.Springs.Split('.')
        |> Seq.filter (fun s -> s <> "")
        |> Seq.toList
        |> String.concat ","

    countArrangements cache (springs, (Size.parse row.Sizes), false)

let unfold row =
    { Springs = row.Springs |> List.replicate 5 |> String.concat "?"
      Sizes = row.Sizes |> List.replicate 5 |> String.concat "," }

let part1 rows =
    rows |> List.map countRowArrangements |> List.sum

let part2 rows =
    rows |> List.map unfold |> List.map countRowArrangements |> List.sum

let run =
    printfn "== Day 12 =="

    let rows = File.ReadLines("inputs/day12.txt") |> Seq.map parseLine |> Seq.toList

    printfn "Part 1: %d" (part1 rows)
    printfn "Part 2: %d" (part2 rows)
    printfn ""
