

open System


type Set = {
    R: int
    G: int
    B: int
}

type Game = {
    Id: int    
    Sets: Set list
}

let parseSet (setStr : string): Set =
    let colors = setStr.Split(',')
    {
        R = colors |> Array.filter (_.Trim().EndsWith("red")) |> Array.map (fun s -> s.Trim().Split(' ').[0] |> Convert.ToInt32) |> Array.sum
        G = colors |> Array.filter (_.Trim().EndsWith("green")) |> Array.map (fun s -> s.Trim().Split(' ').[0] |> Convert.ToInt32) |> Array.sum
        B = colors |> Array.filter (_.Trim().EndsWith("blue")) |> Array.map (fun s -> s.Trim().Split(' ').[0] |> Convert.ToInt32) |> Array.sum
    }

let rec parseGame id (gameStr : string): Game =
    {
        Id = id
        Sets = gameStr.Split(';') |> Array.map parseSet |> Array.toList
    }

let parseLine (line: string) =
     let id = line.Split(':').[0].Split(' ').[1] |> Convert.ToInt32
     parseGame id (line.Split(':').[1])
     
     
let input = System.IO.File.ReadAllLines("input")

let part1 = input
            |> Array.map parseLine
            |> Array.filter (fun x -> List.forall (fun s -> s.R <= 12 && s.G <= 13 && s.B <= 14 ) x.Sets)
            |> Array.sumBy (_.Id)
            
printf $"part1 %A{part1}\n"
let fewestOfEachColor (game: Game) =
    let red = game.Sets |> List.filter (fun x -> x.R > 0 ) |> List.maxBy (_.R) 
    let green = game.Sets |> List.filter (fun x -> x.G > 0 ) |> List.maxBy (_.G)
    let blue = game.Sets |> List.filter (fun x -> x.B > 0 ) |> List.maxBy (_.B)
    {
        R = red.R
        G = green.G
        B = blue.B
    }

let part2 = input
            |> Array.map parseLine
            |> Array.map fewestOfEachColor
            |> Array.map (fun s -> s.R * s.G * s.B)
            |> Array.sum
            
printf $"part2 %A{part2}\n"
            
            
            


