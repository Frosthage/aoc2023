open System
open System.IO

type Card = {
    number: int
    winning: int Set
    my: int Set
}

let parseLine (line:string) =
    let number = line.Split(' ', StringSplitOptions.RemoveEmptyEntries).[1].TrimEnd(':') |> Convert.ToInt32
    let winning = line.Split('|').[0].Split(' ', StringSplitOptions.RemoveEmptyEntries).[2..] |> Array.map Convert.ToInt32 |> Set.ofArray
    let my = line.Split('|').[1].Split(' ', StringSplitOptions.RemoveEmptyEntries ) |> Array.map Convert.ToInt32 |> Set.ofArray
    {number=number; winning=winning; my=my}
    
let input = File.ReadAllLines "input"
let day1 = input
           |> Array.map parseLine
           |> Array.map (fun x -> Set.intersect x.winning x.my)
           |> Array.map (fun x -> pown 2 ((Set.count x ) - 1))
           |> Array.sum
           
printf $"part 1: %A{day1}\n"


let getWins (i: int) (card: Card) =
    let winsCount = Set.intersect card.winning card.my |> Set.count
    (i,if winsCount = 0 then [||] else [|i+1..(i+winsCount)|])
         
let rec traverseWins (map: Map<int, int array>) (i: int) =
    if map.[i].Length = 0 then
        1
    else
        let sum = map.[i] |> Array.map (traverseWins map) |> Array.sum
        sum + 1
        
let cardMap = input
            |> Array.map parseLine
            |> Array.mapi getWins
            |> Array.fold (fun acc (key, value) -> Map.add key value acc) Map.empty
let part2 = input |> Array.map parseLine |> Array.mapi (fun i _ -> traverseWins cardMap i) |> Array.sum

printf $"part 2: %A{part2}\n"

                
                 



