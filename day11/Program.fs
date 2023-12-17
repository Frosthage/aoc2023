open System.IO

let input = File.ReadAllLines("input")

let map  = input
           |> Array.map (_.ToCharArray())

let width = input[0].Length
let emptyRow = Array.create width '.'

let expandEmptySpace (emptyRow: char array) =
    let folder (acc: char[] list) (row: char[]) =
        match row = emptyRow with
        | true -> acc@[row]@[row]
        | false -> acc @ [row]
    folder
    
let expanded1 = map
                |> Array.fold (expandEmptySpace (Array.create input[0].Length '.')) []

let createUniquePairs items =
    [for i in 0 .. (List.length items - 1) do
        for j in (i+1) .. (List.length items - 1) do
            yield (items[i], items[j])]

let shortestDistance (a, b) =
    let x1, y1 = a
    let x2, y2 = b
    abs (x1 - x2) + abs (y1 - y2)

let part1 = expanded1
            |> Array.transpose
            |> Array.fold (expandEmptySpace (Array.create expanded1.Length '.')) []
            |> Array.transpose
            |> Array.mapi (fun y -> Array.mapi (fun x c -> (x,y), c))
            |> Array.map (Array.filter (fun (_, c) -> c <> '.'))
            |> Array.map (Array.map fst)
            |> Array.collect id
            |> Array.toList
            |> createUniquePairs
            |> List.map shortestDistance
            |> List.sum

printfn $"Part1: %A{part1}"


let expandEmptySpace2 =
    let expanded = Array.create emptyRow.Length 'M'
    let folder (acc: char[] list) (row: char[]) =
        match row |> Array.forall (fun c -> c = '.' || c = 'M') with
        | true -> acc@[expanded]
        | false -> acc @ [row]
    folder

let expanded2 = map 
                |> Array.fold expandEmptySpace2 []
                |> Array.transpose
                |> Array.fold expandEmptySpace2 []
                |> Array.transpose

let expand2 m (x, y) =
    let msx = expanded2[0][0..x] |>  Array.filter (fun c -> c = 'M') |> Array.length
    let msy = expanded2[0..y] |> Array.filter (fun c -> c[0] = 'M') |> Array.length
    (x+msx*(m-1), y+msy*(m-1))
    
let part2 = expanded2
            |> Array.mapi (fun y -> Array.mapi (fun x c -> (x,y), c))
            |> Array.map (Array.filter (fun (_, c) -> c = '#'))
            |> Array.map (Array.map fst)
            |> Array.map (Array.map (expand2 1000000))
            |> Array.collect id
            |> Array.toList
            |> createUniquePairs  
            |> List.map shortestDistance
            |> List.map int64 
            |> List.sum
               
printfn $"Part2 %A{part2}"
                


