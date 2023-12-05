module Day02

open System
open System.IO

type Blocks =
    | Red of int
    | Green of int
    | Blue of int

type Handful = Blocks list

type Game = { id: int; handfuls: Handful list }

type Bag = { red: int; green: int; blue: int }

let parseBlocks (s: string) =
    let parts = s.Trim().Split(' ')
    let count = int parts.[0]

    match parts.[1] with
    | "red" -> Red count
    | "green" -> Green count
    | "blue" -> Blue count
    | _ -> failwith "Unknown color"

let parseHandful (s: string) =
    s.Split(',') |> Seq.map parseBlocks |> Seq.toList

let parseGame (s: string) =
    let parts = s.Split(':')
    let gameId = int (parts.[0].Substring(5))
    let handfuls = parts.[1].Split(';') |> Seq.map parseHandful |> Seq.toList
    { id = gameId; handfuls = handfuls }

let isHandfulPossible bag handful =
    handful
    |> List.forall (fun blocks ->
        match blocks with
        | Red(c) -> c <= bag.red
        | Green(c) -> c <= bag.green
        | Blue(c) -> c <= bag.blue)

let isGamePossible bag game =
    game.handfuls |> List.forall (isHandfulPossible bag)

let part1 games =
    let bag = { red = 12; green = 13; blue = 14 }
    games |> Seq.filter (isGamePossible bag) |> Seq.map (fun g -> g.id) |> Seq.sum

let expandBagForBlocks bag blocks =
    match blocks with
    | Red(c) -> { bag with red = Math.Max(bag.red, c) }
    | Green(c) ->
        { bag with
            green = Math.Max(bag.green, c) }
    | Blue(c) ->
        { bag with
            blue = Math.Max(bag.blue, c) }

let rec expandBagForHandful bag handful =
    match handful with
    | [] -> bag
    | blocks :: rest -> expandBagForHandful (expandBagForBlocks bag blocks) rest

let minBagFor game =
    game.handfuls |> List.fold expandBagForHandful { red = 0; green = 0; blue = 0 }

let power bag = bag.red * bag.green * bag.blue

let part2 games =
    games |> Seq.map minBagFor |> Seq.map power |> Seq.sum

let run =
    printfn "== Day 02 =="

    let games = File.ReadLines("inputs/day02.txt") |> Seq.map parseGame |> Seq.toList

    printfn "Part 1: %A" (part1 games)
    printfn "Part 2: %A" (part2 games)
    printfn ""
