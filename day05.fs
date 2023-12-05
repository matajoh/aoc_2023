module Day05

open System.IO

type Range = { Start: uint64; End: uint64 }

type Entry = { Destination: Range; Source: Range }

type Map = { Label: string; Entries: Entry list }

type Almanac = { Seeds: uint64 list; Maps: Map list }

let parseSeeds (line: string) =
    line.Substring(7).Trim().Split(' ')
    |> Array.map (fun s -> s.Trim())
    |> Array.filter (fun s -> s <> "")
    |> Array.map uint64
    |> Array.toList

let parseEntry (line: string) =
    line.Split(' ')
    |> Array.map (fun s -> s.Trim())
    |> Array.filter (fun s -> s <> "")
    |> Array.map uint64
    |> (fun a ->
        { Destination = { Start = a.[0]; End = a.[0] + a.[2] }
          Source = { Start = a.[1]; End = a.[1] + a.[2] } })

let parseMap (lines: string list) =
    { Label = lines.[0]
      Entries = lines |> List.tail |> List.map parseEntry }

let parseMaps (text: string) =
    let parts = text.Split("\n\n") |> Array.map (fun s -> s.Trim()) |> Array.toList
    let seeds = parts.[0] |> parseSeeds

    let maps =
        parts
        |> List.tail
        |> List.map (fun s -> s.Split('\n') |> Array.map (fun s -> s.Trim()) |> Array.toList)
        |> List.map parseMap

    { Seeds = seeds; Maps = maps }

let overlap lhs rhs =
    match max lhs.Start rhs.Start, min lhs.End rhs.End with
    | s, e when e > s && e - s > 0UL -> Some { Start = s; End = e }
    | _ -> None

let map entry seeds =
    let start = entry.Destination.Start + seeds.Start - entry.Source.Start
    let length = seeds.End - seeds.Start
    { Start = start; End = start + length }

let rec mapSeeds seeds overlaps =
    match overlaps with
    | [] when seeds.End > seeds.Start -> [ seeds ]
    | [] -> []
    | (entry, overlap) :: tail ->
        let mapped = map entry overlap
        let remaining = { Start = overlap.End; End = seeds.End }

        if seeds.Start < overlap.Start then
            { Start = seeds.Start
              End = overlap.Start }
            :: mapped
            :: mapSeeds remaining tail
        else
            mapped :: mapSeeds remaining tail

let applyMap map seeds =
    map.Entries
    |> List.map (fun e ->
        match overlap e.Source seeds with
        | Some o -> Some(e, o)
        | None -> None)
    |> List.choose id
    |> List.sortBy (fun (_, o) -> o.Start)
    |> mapSeeds seeds

let getLocations almanac seedRanges =
    almanac.Maps
    |> List.fold (fun acc map -> acc |> List.collect (applyMap map)) seedRanges

let part1 almanac =
    almanac.Seeds
    |> List.map (fun s -> { Start = s; End = s + 1UL })
    |> getLocations almanac
    |> List.map (fun r -> r.Start)
    |> List.min

let rec toRanges seeds =
    match seeds with
    | [] -> []
    | start :: range :: tail -> { Start = start; End = start + range } :: toRanges tail
    | [ _ ] -> failwith "Invalid seeds"

let part2 almanac =
    almanac.Seeds
    |> toRanges
    |> getLocations almanac
    |> List.map (fun r -> r.Start)
    |> List.min

let run =
    printfn "== Day05 =="
    let text = File.ReadAllText("inputs/day05.txt")
    let almanac = parseMaps text

    printfn "Part 1: %i" (part1 almanac)
    printfn "Part 2: %i" (part2 almanac)
    printfn ""
