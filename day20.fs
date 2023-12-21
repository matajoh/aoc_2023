module Day20

open System.IO

open Math

type Pulse =
    | Low of string * string
    | High of string * string

type Module =
    | Broadcaster
    | FlipFlop of bool
    | Conjunction of Map<string, bool>

type IOMap = Map<string, string list>

type Network =
    { Modules: Map<string, Module>
      Inputs: IOMap
      Outputs: IOMap
      Low: int
      High: int
      Activated: bool
      Target: string }

    static member update n k m =
        { n with
            Modules = Map.change k (fun _ -> Some m) n.Modules }

    static member sendLow n i =
        let os = Map.find i n.Outputs
        { n with Low = n.Low + List.length os }, os |> List.map (fun o -> Low(i, o))

    static member sendHigh n i =
        let os = Map.find i n.Outputs

        { n with
            High = n.High + List.length os },
        os |> List.map (fun o -> High(i, o))

    static member score n = n.High * n.Low
    static member activate n = { n with Activated = true }
    static member target n t = { n with Target = t }

let appendOrCreate x xs =
    match xs with
    | Some(xs) -> Some(x :: xs)
    | None -> Some [ x ]

let rec addOutputs inputs outputs i os =
    match os with
    | [] -> inputs, outputs
    | x :: xs ->
        let newInputs = Map.change x (appendOrCreate i) inputs
        let newOutputs = Map.change i (appendOrCreate x) outputs
        addOutputs newInputs newOutputs i xs

let rec parseLines modules inputs outputs (lines: string list) =
    match lines with
    | [] ->
        { Modules = modules
          Inputs = inputs
          Outputs = outputs
          Low = 0
          High = 0
          Activated = false
          Target = "" }
    | x :: xs ->
        let parts = x.Split(" -> ")
        let ms = parts[1].Split(", ") |> Array.toList

        let n, m =
            if parts[0] = "broadcaster" then
                "broadcaster", Broadcaster
            elif parts[0][0] = '%' then
                parts[0][1..], FlipFlop(false)
            elif parts[0][0] = '&' then
                parts[0][1..], Conjunction(Map.empty)
            else
                failwith "Invalid module"

        let newInputs, newOutputs = addOutputs inputs outputs n ms
        parseLines (Map.add n m modules) newInputs newOutputs xs

let parseNetwork lines =
    lines
    |> Seq.toList
    |> parseLines Map.empty ([ ("broadcaster", [ "button" ]) ] |> Map) ([ ("button", [ "broadcaster" ]) ] |> Map)

let updateConjunction memory p =
    match p with
    | Low(i, _) -> Map.change i (fun _ -> Some false) memory
    | High(i, _) -> Map.change i (fun _ -> Some true) memory

let allHigh n k mem =
    Map.find k n.Inputs
    |> List.fold
        (fun acc i ->
            match acc, Map.tryFind i mem with
            | false, _ -> false
            | true, Some(true) -> true
            | true, Some(false) -> false
            | _, None -> false)
        true

let sendPulse n p =
    match p with
    | Low(i, _) when i = n.Target -> (Network.activate n), []
    | Low(_, o) ->
        match Map.tryFind o n.Modules with
        | Some(FlipFlop false) -> Network.sendHigh (Network.update n o (FlipFlop true)) o
        | Some(FlipFlop true) -> Network.sendLow (Network.update n o (FlipFlop false)) o
        | Some(Conjunction oldMem) ->
            let newMem = updateConjunction oldMem p
            Network.sendHigh (Network.update n o (Conjunction newMem)) o
        | Some Broadcaster -> Network.sendLow n o
        | None -> n, []
    | High(_, o) ->
        match Map.tryFind o n.Modules with
        | Some(FlipFlop _) -> n, []
        | Some(Conjunction oldMem) ->
            let newMem = updateConjunction oldMem p

            if allHigh n o newMem then
                Network.sendLow (Network.update n o (Conjunction newMem)) o
            else
                Network.sendHigh (Network.update n o (Conjunction newMem)) o
        | Some Broadcaster -> Network.sendHigh n o
        | None -> n, []

let rec pulse (network, pulses) =
    if List.length pulses = 0 then
        network
    else
        let newNet, sent = sendPulse network (List.head pulses)
        pulse (newNet, (List.tail pulses @ sent))

let rec pressButton c n =
    if c = 0 then
        n
    else
        Network.sendLow n "button" |> pulse |> pressButton (c - 1)

let rec activate c n =
    if n.Activated then
        c
    else
        pressButton 1 n |> activate (c + 1)

let rec allFlipFlops n keys =
    match keys with
    | [] -> true
    | k :: ks ->
        match Map.find k n.Modules with
        | FlipFlop(_) -> allFlipFlops n ks
        | _ -> false

let part1 n = n |> pressButton 1000 |> Network.score

let rec getCounters counters keys net =
    match keys with
    | [] -> counters
    | k :: ks ->
        let inputs = Map.find k net.Inputs

        if inputs |> (allFlipFlops net) then
            getCounters (k :: counters) ks net
        else
            getCounters counters (inputs @ ks) net

let part2 n =
    getCounters [] [ "rx" ] n
    |> List.map (Network.target n)
    |> List.map (activate 0)
    |> List.map uint64
    |> List.fold lcm 1UL

let run =
    printfn "== Day 20 =="

    let network = File.ReadLines("inputs/day20.txt") |> parseNetwork

    printfn "Part 1: %d" (part1 network)
    printfn "Part 2: %d" (part2 network)
    printfn ""
