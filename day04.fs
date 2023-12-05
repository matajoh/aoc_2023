module Day04

open System.IO

type Card =
    { WinningNumbers: Set<int>
      Numbers: int list }

let parseNumbers (line: string) =
    line.Split(' ')
    |> Array.map (fun s -> s.Trim())
    |> Array.filter (fun s -> s <> "")
    |> Array.map int
    |> Array.toList

let parseWinningNumbers (line: string) =
    let parts = line.Split(':')
    parseNumbers parts.[1] |> Set.ofList

let parseCard (line: string) =
    let parts = line.Split('|')

    { WinningNumbers = parseWinningNumbers parts.[0]
      Numbers = parseNumbers parts.[1] }

let parseCards (lines: string list) = lines |> List.map parseCard

let winningCount card =
    card.Numbers
    |> List.sumBy (fun n -> if card.WinningNumbers.Contains(n) then 1 else 0)

let score card =
    let count = winningCount card
    if count = 0 then 0 else 1 <<< (count - 1)

let part1 cards = (cards |> List.map score |> List.sum)

let rec countWinningCards (cards: Card list) (cache: Option<int> array) card =
    match cache.[card] with
    | Some(count) -> count
    | None ->
        let count =
            match winningCount cards.[card] with
            | 0 -> 1
            | count -> [ card + 1 .. card + count ] |> List.map (countWinningCards cards cache) |> List.sum |> (+) 1
        
        cache.[card] <- Some(count)
        count

let part2 cards =
    let cache = Array.create (List.length cards) None
    [ 0 .. List.length cards - 1 ] |> List.map (countWinningCards cards cache) |> List.sum

let run =
    printfn "== Day04 =="
    let cards = File.ReadAllLines "inputs/day04.txt" |> Array.toList |> parseCards

    printfn "Part 1: %i" (part1 cards)
    printfn "Part 2: %i" (part2 cards)
    printfn ""
