open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

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

//let part1 = File.ReadAllLines("input")[0..4]
//            |> Array.map parse
//            |> Array.map (fun x -> (permutate (fst x), snd x))
//            |> Array.map validArrangement
//
//printfn "%A" part1

//Part2
////////////////////

let unfold5 (r: string, g: int list) =
    ($"{r}?{r}?{r}?{r}?{r}", g @ g @ g @ g @ g)
    
let unfold2 (r: string, g: int list) =
    ($"{r}?{r}", g @ g)

let unfold3 (r: string, g: int list) =
    ($"{r}?{r}?{r}", g @ g @ g)

let valid (s: string, groups: int list) =
    let pounds = s.Split(".", StringSplitOptions.RemoveEmptyEntries)

    if pounds.Length <> groups.Length then
        false
    else
        pounds |> Array.map (_.Length) |> Array.toList |> List.forall2 (=) groups

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
    | sh :: st, lh :: lt when sh = lh -> isOrderedSubset st lt
    | _, _ :: lt -> isOrderedSubset smallList lt


let isSubset (s: string, group: int list) =
    let l =
        s.Split('.', StringSplitOptions.RemoveEmptyEntries)
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
    let splits =
        s.ToCharArray()
        |> Array.mapi (fun index value -> (index, value))
        |> Array.filter (fun (_, value) -> value = c)
        |> Array.map fst

    splits.[splits.Length / 2]

let getCenterPeriod (s: string) = getCenter s '.'

let getCenterQuestion (s: string) = getCenter s '?'

let getSpaceRange (s: string) (index: int) =
    let rec expandLeft idx =
        if idx > 0 && s.[idx - 1] = '.' then
            expandLeft (idx - 1)
        else
            idx

    let rec expandRight idx =
        if idx < s.Length - 1 && s.[idx + 1] = '.' then
            expandRight (idx + 1)
        else
            idx

    let left = expandLeft index
    let right = expandRight index
    (left, right)

let combineLists rl rr =
    if List.isEmpty rl && List.isEmpty rr then
        []
    elif List.isEmpty rl then
        rr
    elif List.isEmpty rr then
        rl
    else
        [ for item1 in rl do
              for item2 in rr do
                  yield item1 + item2 ]


let rec recurse3 (s: string) (g: int list) (l: int) (memo: Dictionary<string, string list>) =
    if memo.ContainsKey(s) then
        memo.[s]
    else if s = "" then
        []
    else if s = "?" then
        [ "."; "#" ]
    else if s = "." || s = "#" then
        [ s ]
    else if not (s.Contains("?")) then
        [ s ] |> List.filter (fun x -> isSubset (x, g))
    else if s.Split('.').Length = 2 && s.StartsWith('.') then
        let rr = recurse3 s[1..] g (l + 1) memo
        let strs = combineLists [ "." ] rr

        if l = 0 then
            let apa =
                strs
                |> List.filter (fun x -> x.Length = s.Length)
                |> List.filter (fun x -> valid (x, g))

            apa |> List.sort
        else
            if (s.Contains("?")) then
                memo.Add(
                    s,
                    strs
                    |> List.filter (fun x -> x.Length = s.Length)
                    |> List.filter (fun x -> isSubset (x, g))
                )

            strs
    else if s.Contains('.') then
        let qp = getCenterPeriod s

        let rl = recurse3 s[0 .. qp - 1] g (l + 1) memo
        let rr = recurse3 s[qp..] g (l + 1) memo

        let strs = combineLists rl rr

        if l = 0 then
            let apa =
                strs
                |> List.filter (fun x -> x.Length = s.Length)
                |> List.filter (fun x -> valid (x, g))

            apa
        else
            if (s.Contains("?")) then
                memo.Add(
                    s,
                    strs
                    |> List.filter (fun x -> x.Length = s.Length)
                    |> List.filter (fun x -> isSubset (x, g))
                )

            strs
    else
        let qp = getCenterQuestion s
        let s2 = replaceCharAt qp '.' s
        let s1 = replaceCharAt qp '#' s
        let rr = recurse3 s2 g (l + 1) memo
        let rl = recurse3 s1 g (l + 1) memo

        let strs = rl @ rr

        if l = 0 then
            let apa =
                strs
                |> List.filter (fun x -> x.Length = s.Length)
                |> List.filter (fun x -> valid (x, g))

            apa
        else
            if (s.Contains("?")) then
                memo.Add(
                    s,
                    strs
                    |> List.filter (fun x -> x.Length = s.Length)
                    |> List.filter (fun x -> isSubset (x, g))
                )

            strs


let rec recurse2 (s: string) (g: int list) : int =
    if not (s.Contains('?')) then
        match valid (s, g) with
        | true -> 1
        | false -> 0
    else
        let qp = s.IndexOf '?'
        let s2 = replaceCharAt qp '.' s
        let s1 = replaceCharAt qp '#' s
        (recurse2 s1 g) + (recurse2 s2 g)

let rec recurse2sl (s: string) (g: int list) : string list =
    if not (s.Contains('?')) then
        match valid (s, g) with
        | true -> [s]
        | false -> []
    else
        let qp = s.IndexOf '?'
        let s2 = replaceCharAt qp '.' s
        let s1 = replaceCharAt qp '#' s
        (recurse2sl s1 g) @ (recurse2sl s2 g)




//let part1 = File.ReadAllLines("input")
//            |> Array.map parse
//            |> Array.map (fun x -> (permutate (fst x), snd x))
//            |> Array.map validArrangement
//
//printfn "%A" (part1 |> Array.sum)

//let part23 =
//      File.ReadAllLines("example")[1..1]
//      |> Array.map parse
//      //|> Array.map unfold
//      |> Array.map (fun (s, g) -> recurse2 s g)
//      |> Array.sum

//printf "\n: %A\n" (part23)

//let part22 =
//      File.ReadAllLines("example")[0..]
//      |> Array.map parse
//      //|> Array.map unfold
//      |> Array.map (fun (s, g) -> recurse3 s g 0 (Dictionary<string, string list>()))
//      |> Array.map (fun x -> x.Length)
//
//printf "\n: %A\n" (part22)


//let diff = Array.zip part1 part22
//               |> Array.mapi ( fun i x -> i, x)
//               |> Array.filter (fun (_, (a,b)) -> a <> b)
//
//printfn "%A" (diff)

let rec recurse2b (s: string) (g: int list) : string list =
    if not (s.Contains('?')) then
        match valid (s, g) with
        | true -> [ s ]
        | false -> []
    else
        let qp = s.IndexOf '?'
        let s2 = replaceCharAt qp '.' s
        let s1 = replaceCharAt qp '#' s
        (recurse2b s1 g) @ (recurse2b s2 g)

let file = "input"
let s = 0
let e = 50


//let part2bbb1 =
//      File.ReadAllLines(file)[s..e]
//      |> Array.map parse
//      //|> Array.map (fun (s, g) -> (recurse2b s g), g)
//      |> Array.map (fun (s, g) -> (recurse2b (s) (g)))
//
//printf "%A\n" (part2bbb1 |> Array.map _.Length |> Array.sortDescending)

//let part2ref =
//      File.ReadAllLines(file)[s..e]
//      |> Array.map parse
//      //|> Array.map (fun (s, g) -> (recurse2b s g), g)
//      |> Array.map (fun (s, g) -> (recurse2b (s+"?"+s) (g@g)))
//      |> Array.Parallel.map _.Length
//printf "Ref: %A\n" part2ref

let rec recurse2bbfn (s: string) (i: int) (part1: string list array) (g: int list) =
    if not (s.Contains('?')) then
        part1[i] |> List.map (fun x -> x + s) |> List.filter (fun x -> valid (x, g))
    else
        let qp = s.IndexOf '?'
        let s2 = replaceCharAt qp '.' s
        let s1 = replaceCharAt qp '#' s
        (recurse2bbfn s1 i part1 g) @ (recurse2bbfn s2 i part1 g)

//let part2bbb3 =
//      File.ReadAllLines(file)[s..e]
//      |> Array.map parse
//      |> Array.mapi (fun i (s, g) -> (i , (recurse2bbfn ("?" + s + "?") i part2bbb1 (g@g))))
//      |> Array.map (fun (i, x) -> (part2bbb1[i].Length |> uint64, x.Length |> uint64))
//      |> Array.mapi (fun i (a,b) -> (i, a, b))
//      |> Array.filter (fun (_, a,b) -> (b % a) <> 0UL)
//|> Array.map (fun (a,b) -> (a * (b/a)*(b/a)*(b/a)*(b/a)))

//printf "%A\n" (part2bbb1 |> Array.map (fun x -> x.Length))
//printf "%A\n" part2bbb3
//let sumb = Array.zip (part2bbb1 |> Array.map (fun x -> x.Length |> uint64)) part2bbb3 |> Array.sumBy (fun (a,b) -> (a * (b/a)*(b/a)*(b/a)*(b/a)))
//printf "%A\n" sumb


let isSubset2 (s: string) (g: int list) : bool =
    let springs = s.Split('.') |> Array.map _.Length |> Array.toList |> List.filter (fun x -> x > 0)

    if springs.Length > g.Length then
        false
    else if springs.Length = 1 then
        springs[0] <= g[0]
    else
        let subsetG = g[.. springs.Length - 2]
        let allExceptLast = List.forall2 (=) springs[.. springs.Length - 2] subsetG

        allExceptLast
        && ((springs[.. springs.Length - 2] |> List.last) <= (g |> List.last))


let rec recurseV3 (s: string) (g: int list) (d: int) : string list =
    match s with
    | s when (s = "") -> []
    | s when (s = "?") -> [ "#"; "." ]
    | s when (s[s.Length - 1] = '?') ->
        let ls = s[.. s.Length - 2] + "#"
        let lr = s[.. s.Length - 2] + "."

        let re1 = recurseV3 ls g (d + 1)
        let re2 = recurseV3 lr g (d + 1)

        List.filter (fun x -> isSubset2 x g) (re1 @ re2)
    | s ->
        let re1 = recurseV3 s[.. s.Length - 2] g (d + 1)
        let last_char = s[s.Length - 1]

        re1
        |> List.map (fun x -> $"{x}{last_char}")
        |> List.filter (fun x -> isSubset2 x g)





let rec nextSpring (s: string) (size: int): string list =
    let firstQ = s.IndexOf('?')
    
    let spring = Regex.Match(s, $"^\.*#{{{size}}}(\.|$)")
    
    if size = 0 && s.Contains('?') then
        nextSpring (s |> replaceCharAt firstQ '.') size
    else if size = 0 then
        [s]
    else if spring.Success then
        [spring.Value]
    else if Regex.Match(s,  $"^\.*.{{{size}}}$").Value.Contains('?') then
        (nextSpring (s |> replaceCharAt firstQ '#') size) @
        (nextSpring (s |> replaceCharAt firstQ '.') size)
    else if not(Regex.Match(s,  $"^\.*.{{{size}}}.").Value.Contains('?')) then
        []
    else 
        (nextSpring (s |> replaceCharAt firstQ '#') size) @
        (nextSpring (s |> replaceCharAt firstQ '.') size)

let rec recurseV4 (s: string) (g: int list) (strs: string list) (gs: int list) : string list =
    
    let subStr (s:string) (str: string) =
        if str.Length = 0 then
            s
        else
            s.Substring(str.Length)
    
    match g with
    | [] -> strs
    | x :: xs ->
        let apa = strs
                  |> List.map (fun xx -> (xx,(nextSpring (subStr s xx) x)))
                  |> List.map (fun (s2, strs) -> strs |> List.map (fun x -> s2 + x))
                  |> List.collect id
        
        let newGs = if x <> 0 then gs @ [x] else gs
        
        printf "%A: " (g.Length)
        
        let firstGroups = (newGs @ xs)[0..((newGs @ xs).Length/5-1)]
        let sumSprings = firstGroups |> List.sum
        let copyIndex = newGs.Length / firstGroups.Length
        let firstGroupLength = (s.Length - 4) / 5
        
        let isPossible (ss: string) =
            ss.Length <= (firstGroupLength+1) * (copyIndex+1)
        
        let isPossible2 (ss: string):bool =
             let currentAmountOfPounds = ss.ToCharArray() |> Array.filter (fun x -> x = '#') |> Array.length
             let ssMaxLength = (firstGroupLength+1) * (copyIndex+1) + (copyIndex+1)
             let leftInCopy =  s[ss.Length..(ssMaxLength)]
             let currentAmountOfPoundsInLeft = leftInCopy.ToCharArray() |> Array.filter (fun x -> x = '#' || x = '?') |> Array.length
             
             if ss = "##..#.........#." then
                 currentAmountOfPoundsInLeft + currentAmountOfPounds > ((copyIndex+1)*sumSprings)
             else              
                 currentAmountOfPoundsInLeft + currentAmountOfPounds > ((copyIndex+1)*sumSprings)
                
        let filteredApa = apa
                          |> List.filter (fun x -> valid (x, newGs))
                          |> List.filter isPossible
                          |> List.filter isPossible2
        
        printf "%A\n" filteredApa.Length
        
        recurseV4 s xs filteredApa newGs
        
        
                      
//let apaaa =
//    File.ReadAllLines("input")[0..1]
//    |> Array.map parse
//    //|> Array.map unfold
//    |> Array.map (fun (s, g) -> (recurseV4 s g [""] []).Length, recurse2 s g )
//    |> Array.indexed
//    |> Array.filter (fun (_, (a,b)) -> a <> b)
    
    
//[|(563, 74621); (94, 46128); (620, 24093); (129, 13723); (110, 8484);
//  (846, 8355); (294, 8008); (287, 6746); (662, 6601); (410, 5650)|]

    
//let u1  =
//    File.ReadAllLines("input")[566..566]
//    |> Array.map parse
//    |> Array.map (fun (s, g) -> (s, g @ [0]))
//    |> Array.map (fun (s, g) -> (recurseV4 s g [""] []).Length)
//    //|> Array.indexed
//    //|> Array.sortByDescending snd
//    //|> Array.take 10
//    
//printf "%A" u1
//let u2  =
//    File.ReadAllLines("input")[566..566]
//    |> Array.map parse
//    |> Array.map unfold2
//    |> Array.map (fun (s, g) -> (s, g @ [0]))
//    |> Array.map (fun (s, g) -> (recurseV4 s g [""] []).Length)
//    //|> Array.indexed
//    //|> Array.sortByDescending snd
//    //|> Array.take 10
//
//printf "%A" u2

let u3  =
    File.ReadAllLines("input")[563..563]
    |> Array.map parse
    |> Array.map unfold5
    |> Array.map (fun (s, g) -> (s, g @ [0]))
    |> Array.map (fun (s, g) -> (recurseV4 s g [""] []).Length)
    //|> Array.indexed
    //|> Array.sortByDescending snd
    //|> Array.take 10


printf "%A" u3

//let part2bbb3 =
//      File.ReadAllLines(file)[s..e]
//      |> Array.map parse
//      |> Array.mapi (fun i (s, g) -> (i , recurse2 s g |> uint64, (recurseV4 s g [""] []).Length |> uint64))  
//      |> Array.filter (fun (_, a,b) -> (b % a) <> 0UL)
//      |> Array.map (fun (_, a,b) -> (a * (b/a)*(b/a)*(b/a)*(b/a)))

//printf "%A" part2bbb3
    

    
//".#####.#.#...#.."
//".#####.#..#..#.."
//".#####.#...#.#..";
//".#####..#.#..#.."
//".#####..#..#.#.."
//".#####...#.#.#.."

//[|[".#####.#.#.#."; ".#####.#.#...#."; ".#####.#..#..#."; ".#####.#...#.#.";
//   ".#####..#.#..#."; ".#####..#..#.#."; ".#####...#.#.#."]|]




                      
                  
                      





