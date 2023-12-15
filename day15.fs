module Day15

open System.IO

type Lens = { Label: string; FocalLength: int }

type Step =
    | Remove of string
    | Insert of string * int

let parseStep (step: string) =
    match step.Contains("=") with
    | true ->
        let parts = step.Split('=')
        let label = parts.[0].Trim()
        let focalLength = int (parts.[1].Trim())
        Insert(label, focalLength)
    | false ->
        let label = step.TrimEnd('-')
        Remove(label)

let hash s =
    s |> Seq.fold (fun h c -> ((h + (int c)) * 17) % 256) 0

let rec replace lenses lens acc =
    match lenses, lens with
    | [], None -> List.rev acc
    | [], Some(l) -> List.rev (l :: acc)
    | h :: t, None -> replace t None (h :: acc)
    | h :: t, Some(l) ->
        if h.Label = l.Label then
            replace t None (l :: acc)
        else
            replace t lens (h :: acc)

let operation (boxes: Lens list array) step =
    match step with
    | Remove(label) ->
        let index = hash label
        boxes.[index] <- boxes.[index] |> List.filter (fun b -> b.Label <> label)
    | Insert(label, focalLength) ->
        let index = hash label

        boxes.[index] <-
            replace
                boxes.[index]
                (Some
                    { Label = label
                      FocalLength = focalLength })
                []

let boxFocusingPower i b =
    b |> List.mapi (fun j l -> (i + 1) * (j + 1) * l.FocalLength) |> List.sum

let focusingPower boxes =
    boxes |> Array.mapi boxFocusingPower |> Array.sum

let part1 steps = steps |> Seq.map hash |> Seq.sum

let part2 steps =
    let boxes = Array.init 256 (fun _ -> [])
    steps |> List.map parseStep |> List.iter (operation boxes)
    boxes |> focusingPower

let run =
    printfn "== Day 15 =="

    let steps = File.ReadAllText("inputs/day15.txt").Split(',') |> Seq.toList

    printfn "Part 1: %d" (part1 steps)
    printfn "Part 2: %A" (part2 steps)
