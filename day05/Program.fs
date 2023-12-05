
open System
open System.IO

type Range = {
    sourceStart: int64
    sourceEnd: int64
    destinationStart: int64
}

type FarmMap = {
    name: string
    ranges: Range array
}


let parse (map: string): FarmMap =
    let lines = map.Split('\n')
    let parseRange (line: string) =
        let numbers = line.Split(' ')
        let sourceStart = numbers.[1] |> Convert.ToInt64
        let rangeLength = numbers.[2] |> Convert.ToInt64
        let destinationStart = numbers.[0] |> Convert.ToInt64
        {
            sourceStart = sourceStart
            sourceEnd = sourceStart + rangeLength - 1L 
            destinationStart = destinationStart
        }
    let apa = {
        name = lines.[0]
        ranges = lines.[1..] |> Array.map parseRange        
    }
    apa
    
let input = (File.ReadAllText "input").Split("\n\n")
let seeds = input[0].Split(' ')[1..] |> Array.map Convert.ToInt64

let maps = input.[1..]
           |> Array.map parse
           
let toSoil (farmMap: FarmMap) (seed: int64) =
    let range = farmMap.ranges
                |> Array.filter (fun r -> seed >= r.sourceStart && seed <= r.sourceEnd)
                |> Array.tryHead
    match range with
    | Some r ->  r.destinationStart + (seed - r.sourceStart) 
    | None -> seed


let rec getLocation (maps: FarmMap array) (seed: int64) =
    if maps = [||] then
        seed
    else
        let farmMap = maps |> Array.head
        let soil = toSoil farmMap seed
        getLocation (maps |> Array.tail) soil
        
let rec getLocation2 (maps: FarmMap array) (seed: int64) =
    let mutable location = seed
    for farmMap in maps do
        location <- toSoil farmMap location
    location
let part1 = seeds
            |> Array.map (getLocation2 maps)
            |> Array.min
           
printf "%A\n" part1

let function2 =
    seeds
    |> Array.chunkBySize 2
    |> Array.Parallel.map (fun x ->
        let mutable minValue = Int64.MaxValue
        printf "chunk started\n"
        for j in x.[0] .. x.[0] + x.[1] - 1L do
            let location = getLocation2 maps j 
            minValue <- min minValue location
        printf $"chunk completed: %A{minValue}\n"
        minValue)
    |> Array.min

printf "%A\n" function2
