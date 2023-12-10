module Day07

open System
open System.IO

type Card =
    | Ace = 14
    | King = 13
    | Queen = 12
    | Jack = 11
    | Ten = 10
    | Nine = 9
    | Eight = 8
    | Seven = 7
    | Six = 6
    | Five = 5
    | Four = 4
    | Three = 3
    | Two = 2
    | Joker = 0

type HandType =
    | HighCard = 0
    | OnePair = 1
    | TwoPairs = 2
    | ThreeOfAKind = 3
    | FullHouse = 4
    | FourOfAKind = 5
    | FiveOfAKind = 6

let joker a b = a = Card.Joker || b = Card.Joker

let handType hand =
    let counts =
        hand
        |> Seq.groupBy id
        |> Seq.map (fun (c, cs) -> c, Seq.length cs)
        |> Seq.toList
        |> List.sortBy (fun (c, count) -> (count * -100 + int c))

    match counts with
    | (_, 5) :: [] -> HandType.FiveOfAKind
    | (a, 4) :: (b, 1) :: [] when joker a b -> HandType.FiveOfAKind
    | (a, 3) :: (b, 2) :: [] when joker a b -> HandType.FiveOfAKind
    | (_, 4) :: _ -> HandType.FourOfAKind
    | (a, 3) :: (b, 1) :: _ when joker a b -> HandType.FourOfAKind
    | (a, 2) :: (b, 2) :: _ when joker a b -> HandType.FourOfAKind
    | (_, 3) :: (_, 1) :: _ -> HandType.ThreeOfAKind
    | (a, 2) :: (b, 1) :: _ when joker a b -> HandType.ThreeOfAKind
    | (_, 3) :: (_, 2) :: _ -> HandType.FullHouse
    | (a, 2) :: (_, 2) :: (c, 1) :: [] when joker a c -> HandType.FullHouse
    | (_, 2) :: (_, 2) :: _ -> HandType.TwoPairs
    | (a, 2) :: (b, 1) :: _ when joker a b -> HandType.TwoPairs
    | (_, 2) :: _ -> HandType.OnePair
    | (Card.Joker, 1) :: _ -> HandType.OnePair
    | _ -> HandType.HighCard

let rec compareCards cards =
    match cards with
    | [] -> 0
    | (c0, c1) :: tail ->
        match compare c0 c1 with
        | c when c <> 0 -> c
        | _ -> compareCards tail

[<CustomComparison>]
[<CustomEquality>]
type Hand =
    { Cards: Card array
      Type: HandType }

    interface IComparable with
        member this.CompareTo otherObj =
            let other = otherObj :?> Hand

            match compare this.Type other.Type with
            | c when c <> 0 -> c
            | _ -> compareCards (Seq.zip this.Cards other.Cards |> Seq.toList)

    override this.Equals otherObj =
        let other = otherObj :?> Hand
        compare this other = 0

    override this.GetHashCode() =
        this.Cards
        |> Seq.map (fun c -> int c)
        |> Seq.reduce (fun acc c -> acc * 100 + c)
        |> (+) (int this.Type)

    static member create(cards: Card array) =
        let handType = handType cards
        { Cards = cards; Type = handType }

type Bid = Hand * int

let parseCard joker c =
    match c with
    | 'A' -> Card.Ace
    | 'K' -> Card.King
    | 'Q' -> Card.Queen
    | 'J' -> if joker then Card.Joker else Card.Jack
    | 'T' -> Card.Ten
    | '9' -> Card.Nine
    | '8' -> Card.Eight
    | '7' -> Card.Seven
    | '6' -> Card.Six
    | '5' -> Card.Five
    | '4' -> Card.Four
    | '3' -> Card.Three
    | '2' -> Card.Two
    | _ -> failwith "Invalid card"

let parseBid joker (line: string) =
    let hand = line.[..4] |> Seq.map (parseCard joker) |> Seq.toArray |> Hand.create
    let bid = int line.[6..]
    hand, bid

let winnings joker lines =
    lines
    |> List.map (parseBid joker)
    |> List.sortBy fst
    |> List.map snd
    |> List.mapi (fun i bid -> bid * (i + 1))
    |> List.sum

let part1 lines = winnings false lines

let part2 lines = winnings true lines

let run =
    printfn "== Day 07 =="

    let lines = File.ReadLines "inputs/day07.txt" |> Seq.toList

    printfn "Part 1: %i" (part1 lines)
    printfn "Part 2: %i" (part2 lines)
    printfn ""
