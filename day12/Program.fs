
open System
open System.IO

let parse (line: string) =
    let records = line.Split(" ")
    let groups = records[1].Split(',') |> Array.map int |> Array.toList
    ( records[0], groups )
    
let permutate (input: string) =
    let rec permutateUnknown (s: string) (acc: string list) =
        match s.LastIndexOf '?' with
        | -1 when acc.Length = 0-> [s]
        | -1 -> acc
        | 0 -> ["." + s.[1..s.Length]; "#" + s.[1..s.Length]] @ acc
        | x ->
            let a = s[0..x-1 ] + "." + s[x+1..s.Length]                        
            let b = s[0..x-1] + "#" + s[x+1..s.Length]
            permutateUnknown a acc @ permutateUnknown b acc
    permutateUnknown input []

let validArrangement (permutations: string list, groups: int list) =
    permutations
    |> List.map (_.Split('.', StringSplitOptions.RemoveEmptyEntries))
    |> List.map (Array.map (fun x -> x.Length))
    |> List.filter (fun x -> x |> Array.toList = groups)
    |> List.length
    
//let part1 = File.ReadAllLines("example")
//            |> Array.map parse
//            |> Array.map (fun x -> (permutate (fst x), snd x))
//            |> Array.map validArrangement
//            |> Array.sum
//
//printfn "%A" part1



let unfold (r:string,g:int list) =
    (
        $"{r}?{r}?{r}?{r}?{r}",
        g@g@g@g@g                
    )
   
let part2 = File.ReadAllLines("example")[0..1]
            |> Array.map parse
            |> Array.map (fun x -> (permutate (fst x), snd x))

//printfn "%A" part2


//File.ReadAllLines("example")[1..1]
//|> Array.map parse
//|> Array.map (fun (r,g) -> (permutate r, g))
//|> Array.map (fun (r,g) -> r |> List.map (fun x -> unfold (x,g)) |> List.toArray)
//|> Array.collect id
//|> Array.map (fun (r,g) -> (permutate r, g)) 
//|> Array.map (fun (x,_) -> x |> List.toArray)
//|> Array.collect id
//|> Array.distinct
//|> Array.length




//|> Array.map validArrangement
//|> Array.sum
//|> printfn "%A"




// ???.### 1,1,3
//  1,3,1,6


let rec getGroups (s: string) (group: int list list): int list list=
    let firstQ = s.IndexOf '?'
    let firstP = s.IndexOf '.'
        
    match s with
    | "" ->
        group |> List.map (fun y -> y @ [s.Length])
    | x when x.ToCharArray() |> Array.forall (fun x -> x = '#') ->
        group |> List.map (fun y -> y @ [s.Length])
    | x when firstQ = -1 && firstP > -1 ->
        (getGroups (x[0..firstP-1]) group) @ (getGroups (x[firstP+1..x.Length]) group)
    | x when firstQ > -1 ->
        let a = getGroups (x[0..firstQ-1] + "#" + x[firstQ+1..x.Length]) group
        let b = getGroups (x[0..firstQ-1] + "." + x[firstQ+1..x.Length]) group
        a @ b
    | _ -> raise <| Exception("Not implemented")

getGroups "###"  [[]] |> printfn "%A"







            





