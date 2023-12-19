module Day19

open System.IO

type Category =
    | X
    | M
    | A
    | S

type Rule =
    | Default of string
    | LessThan of Category * int * string
    | GreaterThan of Category * int * string

type Workflow = { Name: string; Rules: Rule list }

type Part =
    { X: int
      M: int
      A: int
      S: int }

    static member zero = { X = 0; M = 0; A = 0; S = 0 }
    static member min = { X = 1; M = 1; A = 1; S = 1 }

    static member max =
        { X = 4000
          M = 4000
          A = 4000
          S = 4000 }

    static member get p c =
        match c with
        | X -> p.X
        | M -> p.M
        | A -> p.A
        | S -> p.S

    static member set p c v =
        match c with
        | X -> { p with X = v }
        | M -> { p with M = v }
        | A -> { p with A = v }
        | S -> { p with S = v }

    static member ratings p = p.X + p.M + p.A + p.S

type PartRange =
    { Start: Part
      End: Part }

    static member split o r =
        match o with
        | Default(t) -> Some(r, t), None
        | LessThan(c, v, t) ->
            if Part.get r.Start c >= v then
                None, Some r
            elif Part.get r.End c < v then
                Some(r, t), None
            else
                Some(
                    { r with
                        End = Part.set r.End c (v - 1) },
                    t
                ),
                Some { r with Start = Part.set r.Start c v }
        | GreaterThan(c, v, t) ->
            if Part.get r.End c <= v then
                None, Some r
            elif Part.get r.Start c > v then
                Some(r, t), None
            else
                Some(
                    { r with
                        Start = Part.set r.Start c (v + 1) },
                    t
                ),
                Some { r with End = Part.set r.End c v }

    static member count r =
        [ X; M; A; S ]
        |> List.map (fun c -> (Part.get r.End c) - (Part.get r.Start c) + 1)
        |> List.map uint64
        |> List.fold (*) 1UL

let parseRule rule =
    if String.exists ((=) ':') rule then
        let parts = rule.Split(':')

        let c =
            match parts[0][0] with
            | 'x' -> X
            | 'm' -> M
            | 'a' -> A
            | 's' -> S
            | _ -> failwith "Invalid category"

        let v = int (parts[0][2..])
        let t = parts[1]

        match parts[0][1] with
        | '<' -> LessThan(c, v, t)
        | '>' -> GreaterThan(c, v, t)
        | _ -> failwith "Invalid op"
    else
        Default(rule)

let parseWorkflow (line: string) =
    let index = line.IndexOf('{')
    let name = line[.. index - 1]

    let rules =
        line[index + 1 .. line.Length - 2].Split(',')
        |> Array.map parseRule
        |> Array.toList

    { Name = name; Rules = rules }

let rec parseCategories p (categories: string list) =
    match categories with
    | [] -> p
    | c :: cs when c.StartsWith("x=") -> parseCategories { p with X = int c[2..] } cs
    | c :: cs when c.StartsWith("m=") -> parseCategories { p with M = int c[2..] } cs
    | c :: cs when c.StartsWith("a=") -> parseCategories { p with A = int c[2..] } cs
    | c :: cs when c.StartsWith("s=") -> parseCategories { p with S = int c[2..] } cs
    | _ -> failwith "Invalid category"

let parsePart (line: string) =
    line[1 .. line.Length - 2].Split(',')
    |> Array.toList
    |> parseCategories Part.zero

let rec parseWorkflows workflows lines =
    match lines with
    | [] -> failwith "Invalid state"
    | "" :: tail -> (List.rev workflows), tail
    | line :: tail -> parseWorkflows ((parseWorkflow line) :: workflows) tail

let parse lines =
    let workflows, parts = parseWorkflows [] lines
    workflows |> List.map (fun w -> w.Name, w) |> Map, parts |> List.map parsePart

let rec workflow rules p =
    match rules with
    | Default(t) :: [] -> t
    | LessThan(c, v, t) :: rs -> if Part.get p c < v then t else workflow rs p
    | GreaterThan(c, v, t) :: rs -> if Part.get p c > v then t else workflow rs p
    | _ -> failwith "Invalid rules"

let rules w n = (Map.find n w).Rules

let rec processPart w n p =
    match n with
    | "A" -> Some p
    | "R" -> None
    | _ -> processPart w (workflow (rules w n) p) p

let rec workflowRanges ranges range rules =
    match rules with
    | [] -> ranges
    | rule :: tail ->
        match PartRange.split rule range with
        | None, Some _ -> workflowRanges ranges range tail
        | Some x, None -> x :: ranges
        | Some x, Some y -> workflowRanges (x :: ranges) y tail
        | _ -> failwith "Invalid state"

let rec processRanges accepted workflows ranges =
    match ranges with
    | [] -> accepted
    | (r, "A") :: rs -> processRanges (r :: accepted) workflows rs
    | (_, "R") :: rs -> processRanges accepted workflows rs
    | (r, n) :: rs -> processRanges accepted workflows ((workflowRanges [] r (rules workflows n)) @ rs)

let part1 workflows parts =
    parts
    |> List.choose (processPart workflows "in")
    |> List.map Part.ratings
    |> List.sum

let part2 workflows =
    [ { Start = Part.min; End = Part.max }, "in" ]
    |> processRanges [] workflows
    |> List.map PartRange.count
    |> List.sum

let run =
    printfn "== Day 19 =="

    let workflows, parts = File.ReadLines("inputs/day19.txt") |> Seq.toList |> parse

    printfn "Part 1: %d" (part1 workflows parts)
    printfn "Part 2: %d" (part2 workflows)
    printfn ""
