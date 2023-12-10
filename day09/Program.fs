
open System
open System.IO

let input = File.ReadAllLines("input")
            |> Array.toList
            |> List.map (fun x -> x.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.toList)
            |> List.map (List.map int)


let rec getDiffs (x:int List) (r: int List List) =
    let w = List.windowed 2 x |> List.toArray
    match w with
    | _ when x |> List.forall (fun x -> x = 0) -> r
    | _ ->
        let diffs = w |> Array.map (fun x -> x[1] - x[0]) |> Array.toList
        getDiffs diffs ([diffs] @ r)


let extrapolate (l: int list list) =
    let lastValues = l |> List.map (fun x -> x |> List.last)
    List.scan (fun acc x -> acc + x ) 0 lastValues
        

let part1 = input
          |> List.map (fun x -> getDiffs x [x])
          |> List.map extrapolate
          |> List.map (fun x -> x |> List.last)
          |> List.sum
 
printf "%A\n" part1

let extrapolate2 (l: int list list) =
    let firstValue = l |> List.map (fun x -> x |> List.head)
    List.scan (fun acc x -> x - acc ) 0 firstValue

let part2 = input
          |> List.map (fun x -> getDiffs x [x])
          |> List.map extrapolate2
          |> List.map (fun x -> x |> List.last)
          |> List.sum
 
printf "%A" part2