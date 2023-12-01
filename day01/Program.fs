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

let numericalDigits = [|"1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9";|]
let wordDigits =  [|"one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"|]

let rec findDigits (line: string) (result: List<string>) : List<string> =
    match line with
    | "" -> result
    | _ when Array.exists (fun (x:string) -> line.StartsWith x) numericalDigits ->
        findDigits (line.Substring(1)) (result @ [Array.find(fun (x:string) -> line.StartsWith x) numericalDigits])
    | line when Array.exists (fun (x:string) -> line.StartsWith x) wordDigits -> 
        findDigits (line.Substring(1)) (result @ [Array.find(fun (x:string) -> line.StartsWith x) wordDigits])
    | _ -> findDigits (line.Substring(1)) result
        
let getDigits line =
    findDigits line []
    
let convertToNumber (digit: string) =
    let wordDigit = Array.tryFindIndex (fun (x:string) -> x = digit) wordDigits
    let numericalDigit = Array.tryFindIndex (fun (x:string) -> x = digit) numericalDigits
    
    if wordDigit.IsSome then
        wordDigit.Value + 1
    else
        numericalDigit.Value + 1    
        
    
    
let firstAndLastDigit (digits: string list) =
    let first = digits |> List.head |> convertToNumber 
    let last = digits |> List.last |> convertToNumber
    first * 10 + last
    
let digits = getDigits "1dfsgdtwodkfgj321six394875six" |> firstAndLastDigit

let lines2 = System.IO.File.ReadLines("input2")
let part2 = lines2
                    |> Seq.map getDigits
                    |> Seq.map firstAndLastDigit
                    |> Seq.sum

printfn $"Part 2: %A{part2}"

     
        
                 

                 





