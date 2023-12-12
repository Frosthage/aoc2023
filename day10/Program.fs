open System.IO

let file = "input"
            
type Edge = {
    _to: float*float
}

type Node = {
    position: int*int
    edges: (int*int) list
    c: char

}

let toNode (p: (int*int)*char): Node =
    let x,y = fst p 
        
    let edges = match snd p with
                | 'S' -> []
                | '|' -> [(x,y-1);(x,y+1)]
                | '-' -> [(x-1,y);(x+1,y)]
                | 'L' -> [(x,y-1);(x+1,y)]
                | 'J' -> [(x,y-1);(x-1,y)]
                | '.' -> []
                | '7' -> [(x-1,y);(x,y+1)]
                | 'F' -> [(x+1,y);(x,y+1)]
                | _ -> failwith "missing tile"
    {
        position = fst p
        edges = edges
        c = snd p
    }




let tiles2 = File.ReadAllLines(file)
             |> Array.mapi (fun i x -> (i, x))
             |> Array.map (fun (i, x) -> x.ToCharArray() |> Array.mapi (fun j y -> ((j, i), y)))
             |> Array.collect id
             |> Array.map toNode
                          
let sIndex = tiles2 |> Array.findIndex (fun x -> x.c = 'S')
let s = tiles2[sIndex]
let edges = tiles2 |> Array.filter (fun x -> x.edges |> List.contains s.position) |> Array.map _.position

tiles2[sIndex] <- { s with edges = Array.toList edges} 

let tilesMap = tiles2 |> Array.fold (fun acc x -> Map.add x.position x acc) Map.empty

let rec traverse (position: int*int) (from: int*int) distance =
    let node = tilesMap[position]
    let next = node.edges |> List.find (fun x -> x <> from)        
    match node.c with
    | 'S' -> distance + 1
    | _ -> traverse next position (distance + 1)
    
let part1 = traverse edges[0] s.position 0
                    
printf "Part1: %A\n" (part1 / 2)

let rec getVertices (position: int*int) (from: int*int) vertices =
    let node = tilesMap[position]
    let next = node.edges |> List.find (fun x -> x <> from)        
    match node.c with
    | 'S' -> vertices
    | _ -> getVertices next position [node] @ vertices
    
let vertices = [s] @ (getVertices edges[0] s.position []) @ [s]

let polygonEdges = vertices
                    |> List.windowed 2
                    |> List.map (fun x -> x[0].position, x[1].position)
                    |> List.map (fun ((x1,y1),(x2,y2)) -> ((float x1, float y1), (float x2, float y2)))
let pointInPolygon (p: float*float) =
    let found = polygonEdges |> List.tryFind (fun p1 -> p = fst p1 || p = snd p1 )
    let x,y = p
    let intersect = polygonEdges
                    |> List.filter (fun ((_,y1),(_,y2)) -> y1 <> y2)
                    |> List.filter (fun ((x1,_),(x2,_)) -> x1 = x2)
                    |> List.filter (fun ((x1,y1), (x2,y2)) ->
                        ((x1 > x && y1 < y+0.5) && (x2 > x && y2 > y+0.5)) ||
                        ((x1 > x && y1 > y+0.5) && (x2 > x && y2 < y+0.5)))
    
    found.IsNone && (intersect |> List.length) % 2 = 1
        
let path = vertices |> List.map _.position |> Set.ofList
            
let part2 = File.ReadAllLines(file)
            |> Array.mapi (fun i x -> (i, x))
            |> Array.map (fun (i, x) -> x.ToCharArray() |> Array.mapi (fun j _ -> (j, i)))
            |> Array.collect id
            |> Array.map (fun p -> (p,pointInPolygon (float (fst p), float (snd p))))
            |> Array.filter (fun (_,b) -> b)
            |> Array.length


printf "Part2: %A\n" part2

