

open System.IO

type HandStrength =
    | FiveOfAKind 
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard
    
type CardAndBid = {
    hand: string
    bid: int
}

let input = File.ReadAllLines("input")

let parse (s: string): CardAndBid =
    let parts = s.Split(' ')
    { hand = parts.[0]; bid = int parts.[1] }

let toStrength (c: CardAndBid): HandStrength =
    let cards = c.hand |> Seq.groupBy id
    let counts = cards |> Seq.length
    
    match counts with
    | 1 -> FiveOfAKind
    | 2 ->
        match Seq.head cards |> snd |> Seq.length with
        | 4 -> FourOfAKind
        | 1 -> FourOfAKind
        | 3 -> FullHouse
        | 2 -> FullHouse
        | _ -> failwith "Invalid hand"
    | 3 ->
        match cards |> Seq.map (fun (_, v) -> v |> Seq.length) |> Seq.max with
        | 3 -> ThreeOfAKind
        | 2 -> TwoPair
        | _ -> failwith "invalid hand"
    | 4 -> OnePair
    | 5 -> HighCard
    | _ -> failwith "Invalid hand"

let typeValue (h: HandStrength): int =
    match h with
    | FiveOfAKind -> 7
    | FourOfAKind -> 6
    | FullHouse -> 5
    | ThreeOfAKind -> 4
    | TwoPair -> 3
    | OnePair -> 2
    | HighCard -> 1
    

let cardStrength (c: char): int =
    match c with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | _ -> int c - int '0'
    

let handComparer(c: CardAndBid) (c2: CardAndBid) =
    let h1 = c |> toStrength |> typeValue
    let h2 = c2 |> toStrength |> typeValue
    
    if h1 = h2 then
        let value = Array.zip (c.hand.ToCharArray()) (c2.hand.ToCharArray())
                  |> Array.map(fun (c1, c2) -> cardStrength c1 - cardStrength c2)
                  |> Array.filter(fun x -> x <> 0)
                  |> Array.head
        value               
    else
        h1 - h2
        
    
let part1 = input
          |> Array.map parse
          |> Array.sortWith handComparer
          |> Array.mapi (fun i c -> (i + 1) * c.bid)
          |> Array.sum
          
          
printfn "%d" part1

let toStrength2 (c: CardAndBid): HandStrength =
    let cards = c.hand |> Seq.groupBy id
    let counts = cards |> Seq.length
    
    
    let strength = match counts with
                    | 1 -> FiveOfAKind
                    | 2 ->
                        match Seq.head cards |> snd |> Seq.length with
                        | 4 -> FourOfAKind
                        | 1 -> FourOfAKind
                        | 3 -> FullHouse
                        | 2 -> FullHouse
                        | _ -> failwith "Invalid hand"
                    | 3 ->
                        match cards |> Seq.map (fun (_, v) -> v |> Seq.length) |> Seq.max with
                        | 3 -> ThreeOfAKind
                        | 2 -> TwoPair
                        | _ -> failwith "invalid hand"
                    | 4 -> OnePair
                    | 5 -> HighCard
                    | _ -> failwith "Invalid hand"
    let js = cards |> Seq.tryFind (fun (c, v) -> c = 'J') |> Option.map (fun (_, v) -> v |> Seq.length) |> Option.defaultValue 0
    
    match (strength, js) with
    //| FiveOfAKind, 0 -> FiveOfAKind
    | FiveOfAKind, 5 -> FiveOfAKind
    | FourOfAKind, 1 -> FiveOfAKind
    | FourOfAKind, 4 -> FiveOfAKind
    | FullHouse, 2 -> FiveOfAKind
    | FullHouse, 3 -> FiveOfAKind
    | ThreeOfAKind, 1 -> FourOfAKind
    | ThreeOfAKind, 3 -> FourOfAKind
    | TwoPair, 2 -> FourOfAKind
    | TwoPair, 1 -> FullHouse
    | OnePair, 1 -> ThreeOfAKind
    | OnePair, 2 -> ThreeOfAKind
    | HighCard, 1 -> OnePair
    | x, 0 -> x
    | x,y  -> failwith $"{c.hand} {x} {y} "
    
let cardStrength2 (c: char): int =
    match c with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> -1
    | 'T' -> 10
    | _ -> int c - int '0'

let handComparer2(c: CardAndBid) (c2: CardAndBid) =
    let h1 = c |> toStrength2 |> typeValue
    let h2 = c2 |> toStrength2 |> typeValue
    
    try 
        if h1 = h2 then
            let value = Array.zip (c.hand.ToCharArray()) (c2.hand.ToCharArray())
                      |> Array.map(fun (c1, c2) -> cardStrength2 c1 - cardStrength2 c2)
                      |> Array.filter(fun x -> x <> 0)
                      |> Array.tryHead
            value |> Option.defaultValue 0
        else
            h1 - h2
    with
    | :? System.Exception as ex -> 
        printfn "%A %A" c c2
        failwith "sdfsfd"
    


let part2 = input
          |> Array.map parse
          |> Array.sortWith handComparer2
          |> Array.mapi (fun i c -> (i + 1) * c.bid)
          |> Array.sum

          
printf "%A" part2






