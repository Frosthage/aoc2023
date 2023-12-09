open System
open System.IO

let input = File.ReadAllLines("input")
let path = input.[0]

let theMap = input
            |> Array.skip 2
            |> Array.map (fun line ->
                let split = line.Split('=', StringSplitOptions.TrimEntries)
                let name = split.[0]
                (name, (split.[1][1..3], split.[1].[6..8])))
            |> Array.fold (fun acc (key, value) -> Map.add key value acc) Map.empty

let loop =
    seq {
        let mutable i = 0
        while true do
            for x in path do
                yield x
    }


let scanner (state: string) (lr:char) =
    let l, r = theMap.[state]
    match lr with
    | 'L' -> l
    | 'R' -> r
    | _ -> failwith "unknown direction"

let part1 = loop |> Seq.scan scanner "AAA" |> Seq.takeWhile (fun x -> x <> "ZZZ") |> Seq.length

printfn "part1 %A" part1

let start2 = theMap.Keys |> Seq.filter (fun k -> k.EndsWith "A") |> Seq.toArray

let primeFactors n =
    let rec loop n divisor factors =
        if n = 1 then
            factors
        elif n % divisor = 0 then
            loop (n / divisor) divisor (divisor :: factors)
        else
            loop n (divisor + 1) factors
    loop n 2 []

let part2 = start2 |>
            Array.map (fun s ->
                loop
                |> Seq.scan scanner s
                |> Seq.takeWhile (fun x -> x.EndsWith("Z") = false) 
                |> Seq.length
                )
            |> Array.map (fun x -> primeFactors x)
            |> Array.collect (fun x -> x |> List.toArray)
            |> Array.map (fun x -> Convert.ToUInt64(x))
            |> Array.distinct 
            |> Array.reduce (*)

printfn "part2 %A\n" part2
