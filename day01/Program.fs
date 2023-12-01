// For more information see https://aka.ms/fsharp-console-apps


open System
open System.Linq

let lines = System.IO.File.ReadLines("input1")

let digits_only (s:string) = 
    s.ToCharArray()
    |> Array.filter (fun c -> Char.IsDigit(c))
    |> Array.map (_.ToString())

let part1 = lines
                 |> Seq.map digits_only
                 |> Seq.map (fun x -> $"{x[0]}{x[x.Length-1]}")
                 |> Seq.map Convert.ToInt32
                 |> Seq.sum

printfn $"Part 1: %A{part1}"

let numMap = [
    ("1", 1);
    ("2", 2);
    ("3", 3);
    ("4", 4);
    ("5", 5);
    ("6", 6);
    ("7", 7);
    ("8", 8);
    ("9", 9);
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9) ] |> Map.ofList

let rec findDigits (line: string) (result: List<int>) : List<int> =
    
    let digit = Seq.tryFind (fun (x:string) -> line.StartsWith x) numMap.Keys
    
    match line with
    | "" -> result
    | _ when digit.IsSome -> findDigits (line.Substring(1)) (result @ [numMap.[digit.Value]])
    | _ -> findDigits (line.Substring(1)) result
    
        
let getDigits line =
    findDigits line []
    
    
let lines2 = System.IO.File.ReadLines("input2")
let part2 = lines2
                    |> Seq.map getDigits
                    |> Seq.map (fun x -> (List.head x, List.last x))
                    |> Seq.map (fun (x, y) -> x * 10 + y)
                    |> Seq.sum
                    

printfn $"Part 2: %A{part2}"

     
        
                 
                 





