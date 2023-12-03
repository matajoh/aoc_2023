﻿let run i =
    match i with
    | 1 -> Day01.run
    | 2 -> Day02.run
    | _ -> printfn "Day %i not implemented" i

[<EntryPoint>]
let main argv =
    printfn "Advent of Code 2023"
    
    match argv.Length with
    | 0 -> [1..2] 
        |> Seq.iter (fun i -> run i)
    | _ -> argv
        |> Seq.map int
        |> Seq.iter (fun i -> run i)
    0
