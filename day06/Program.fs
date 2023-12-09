

let example = [|
    (7L,9L);
    (15L,40L);
    (30L,200L)
|]

let input = [|
     (57L, 291L);
     (72L, 1172L);
     (69L, 1176L);
     (92L, 2026L)
|]

let input2 = [|
     (57726992L, 291117211762026L)
|]




let rec infiniteIntegers n = 
    seq {
        yield n
        yield! infiniteIntegers (n + 1)
    }

let isRecord (race: int64*int64) (hold:int64) =
    let timeToDrive = fst race - hold
    timeToDrive * hold


let part1 = input
           |> Seq.map (fun x->
                infiniteIntegers 1 
                |> Seq.map (fun y -> isRecord x y)
                |> Seq.takeWhile (fun y -> y > 0)
                |> Seq.toArray
                |> Array.filter (fun y -> y >= snd x)
                |> Array.length
                )
           |> Seq.reduce (*)

let part2 = input2
           |> Seq.map (fun x->
                infiniteIntegers 1 
                |> Seq.map (fun y -> isRecord x y)
                |> Seq.takeWhile (fun y -> y > 0)
                |> Seq.toArray
                |> Array.filter (fun y -> y >= snd x)
                |> Array.length
                )
           |> Seq.reduce (*)


          
printfn $"part1: %A{part1}\n"
printfn $"part2: %A{part2}\n"
            