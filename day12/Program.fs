open System
open System.IO
open Microsoft.FSharp.Collections
open Microsoft.VisualBasic

let parse (line: string) =
    let records = line.Split(" ")
    let groups = records[1].Split(',') |> Array.map int |> Array.toList
    (records[0], groups)

let permutate (input: string) =
    let rec permutateUnknown (s: string) (acc: string list) =
        match s.LastIndexOf '?' with
        | -1 when acc.Length = 0 -> [ s ]
        | -1 -> acc
        | 0 -> [ "." + s.[1 .. s.Length]; "#" + s.[1 .. s.Length] ] @ acc
        | x ->
            let a = s[0 .. x - 1] + "." + s[x + 1 .. s.Length]
            let b = s[0 .. x - 1] + "#" + s[x + 1 .. s.Length]
            permutateUnknown a acc @ permutateUnknown b acc

    permutateUnknown input []

let validArrangement (permutations: string list, groups: int list) =
    permutations
    |> List.map (_.Split('.', StringSplitOptions.RemoveEmptyEntries))
    |> List.map (Array.map (fun x -> x.Length))
    |> List.filter (fun x -> x |> Array.toList = groups)
    |> List.length

//let part1 = File.ReadAllLines("input")
//            |> Array.map parse
//            |> Array.map (fun x -> (permutate (fst x), snd x))
//            |> Array.map validArrangement
//            |> Array.sum
//
//printfn "%A" part1

//Part2
////////////////////

let unfold (r: string, g: int list) =
    ($"{r}?{r}?{r}?{r}?{r}", g @ g @ g @ g @ g)


let valid (s: string, groups: int list) =
    s.Split(".", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (_.Length)
    |> Array.toList
    |> List.forall2 (=) groups

let part2 =
    File.ReadAllLines("example")[0..1]
    |> Array.map parse
    |> Array.map unfold


//printf
//    "%A"
//    ([ ("#.#.###", [ 1; 1; 3 ])
//       (".#...#....###.", [ 1; 1; 3 ])
//       (".#.###.#.######", [ 1; 3; 1; 6 ])
//       ("####.#...#...", [ 4; 1; 1 ])
//       ("#....######..#####.", [ 1; 6; 5 ])
//       (".###.##....#", [ 3; 2; 1 ]) ]
//     |> List.forall valid)
//
//
//printf
//    "%A"
//    ([ ("#.#.###", [ 1; 1; 3 ])
//       (".#...#....###.", [ 1; 1; 3 ])
//       (".#.###.#.######", [ 1; 3; 1; 6 ])
//       ("####.#...#...", [ 4; 1; 1 ])
//       ("#....######..#####.", [ 1; 6; 5 ])
//       (".###.##....#", [ 3; 2; 1 ]) ]
//     |> List.map unfold
//     |> List.forall valid)

let rec isOrderedSubset smallList largeList = 
    match smallList, largeList with
    | [], _ -> true
    | _, [] -> false
    | sh::st, lh::lt when sh = lh -> isOrderedSubset st lt
    | _, _::lt -> isOrderedSubset smallList lt
    

let isSubset (s: string, group: int list) =
    let l = s.Split('.', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (_.Length)
            |> Array.toList
    isOrderedSubset l group
    
    
    
let replaceCharAt (index: int) (newChar: char) (input: string) : string =
    if index < 0 || index >= input.Length then
        invalidArg "index" "Index is out of bounds."
    else
        let prefix = input.Substring(0, index)
        let suffix = input.Substring(index + 1)
        prefix + string newChar + suffix


let getCenter (s: string) (c: char) =
    let splits = s.ToCharArray()
                 |> Array.mapi (fun index value -> (index, value))
                 |> Array.filter (fun (_, value) -> value = c)
                 |> Array.map fst
            
    splits.[splits.Length / 2]

let getCenterPeriod (s: string) =
    getCenter s '.'

let getCenterQuestion (s: string) =
    getCenter s '?'
    

let rec recurs (s: string) (group: int list) =
    if s = "" then []
    else if s = "?" then ["#"; "."]
    else if not (s.Contains("?")) then
        let l = s.Split('.', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (_.Length)
                |> Array.toList
        if isOrderedSubset l group then
            [s]                        
        else
            []
    else if s.Contains(".") then
        let splits = s.ToCharArray()
                     |> Array.mapi (fun index value -> (index, value))
                     |> Array.filter (fun (_, value) -> value = '.')
                     |> Array.map fst
                
        let midIndex =  splits.[splits.Length / 2]
        let x1 = recurs (s.Substring(0, midIndex)) group
        let x2 = recurs (s.Substring(midIndex+1)) group
        
        let strs = [ for item1 in x1 do
                              for item2 in x2 do
                                  yield item1+item2]
        strs
        
    else
        let splits = s.ToCharArray()
                     |> Array.mapi (fun index value -> (index, value))
                     |> Array.filter (fun (_, value) -> value = '?')
                     |> Array.map fst
       
        let midIndex =  splits.[splits.Length / 2]
        
        let x1 = recurs (s.Substring(0, midIndex)) group
        let x2 = recurs ("#" + s.Substring(midIndex+1)) group
        let x3 = recurs ("." + s.Substring(midIndex+1)) group
        
        x1 @ x2 @ x3
        

        
        
let rec recurs2 (s: string) (group: int list) =
    if s = "" then
        []
    else if s = "?" then
        [".";"#"]
    else if s |> Seq.distinct |> Seq.toArray |> String = "#" then
        if isSubset (s, group) then
            [s]
        else
            []
    else if not (s.Contains('?')) then
        let cp = getCenterPeriod s
        let l = recurs2 s[0..cp-1] group
        let r = recurs2 s[cp+1..] group
        
        let strs = [ for item1 in l do
                              for item2 in r do
                                  yield item1+"."+item2]
        strs |> List.filter (fun x -> isSubset (x, group))
    else 
        let qp = getCenterQuestion s
        let sl = replaceCharAt qp '#' s
        let sr = replaceCharAt qp '.' s
        let l = recurs2 sl group
        let r = recurs2 sr group
        let strs = [ for item1 in l do
                              for item2 in r do
                                  yield item1+item2]
        strs |> List.filter (fun x -> isSubset (x, group))

//printf "%A" (getCenterQuestion "?.#.###" )

//printf "%A" (recurs2 "#.#.###" [1;1;3])

printf "%A" (recurs2 "???.###" [1;1;3])
printf "%A" (recurs2 ".??..??...?##." [1;1;3])



let part2r =
    File.ReadAllLines("example")
    |> Array.map parse
    |> Array.map (fun (s, g) -> recurs2 s g)
    |> Array.sumBy (_.Length)


printf "%A" part2r
        
        
                
                        
    
        
        
                
        
        
        
    
    
        


