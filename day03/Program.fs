open System
open System.IO
open System.Text.RegularExpressions


type Number = {
    row: int
    start: int
    end_: int
    value: int
}

type Star = {
    row: int
    col: int
}

let extractNumbers (i:int, row: string) =
    Regex.Matches(row, @"\d+")
    |> Seq.cast<Match>
    |> Seq.map (fun m -> {row = i; start = m.Index; end_ = m.Index + m.Length-1; value = Convert.ToInt32(m.Value)})
    |> Seq.toList
   
    
let hasAdjacentSymbol (input: string array, n: Number) =
    let left = max 0 (n.start - 1)
    let right = min (n.end_ + 1) (input.[n.row].Length - 1)
            
    let above = if n.row > 0 then input.[n.row - 1].[left..right] else ""
    let below = if n.row < input.Length - 1 then input.[n.row + 1].[left..right] else ""
    let current = input.[n.row].[left..right]
    
    let all = above + below + current
    
    not (Seq.forall (fun c -> (c >= '0' && c <= '9') || c = '.') all)

let input = File.ReadAllLines("input")
let part1 = input
              |> Seq.mapi (fun i row -> extractNumbers(i, row))
              |> Seq.collect id
              |> Seq.filter (fun n -> hasAdjacentSymbol(input, n))
              |> Seq.sumBy _.value
              
              
printf $"Part1: %A{part1}\n"

let numbers = input
              |> Seq.mapi (fun i row -> extractNumbers(i, row))
              |> Seq.collect id
              |> Seq.toList

let starAdjacentToTwoNumbers (numbers: Number list, star: Star) =
    let isAdjacent (n: Number) =
        n.start - 1 <= star.col && n.end_ + 1 >= star.col && n.row - 1 <= star.row && n.row + 1 >= star.row
            
    numbers |> Seq.filter isAdjacent |> Seq.toList
        
let stars = input
             |> Seq.mapi (fun i row -> Regex.Matches(row, @"\*")
                                       |> Seq.cast<Match>
                                       |> Seq.map (fun m -> {row = i; col = m.Index})
                                       |> Seq.toList)
             |> Seq.collect id
             |> Seq.map (fun x -> starAdjacentToTwoNumbers(numbers, x))
             |> Seq.filter (fun x -> x.Length = 2)
             |> Seq.map (fun x -> x[0].value * x[1].value)
             |> Seq.sum

printf $"Part2: %A{stars}"
              
              
    
    

