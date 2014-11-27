open Symbols
open System

let makeCharSeq (characters: char seq) elLen =
    let computeVariants (st: char seq seq) =
        characters |> Seq.map (fun ch -> Seq.map (fun s -> (Seq.append (seq[ch]) s)) st) |> Seq.concat
        
    let rec makeSeqLevel (subSeq: char seq seq) currentLevel = 
        if currentLevel = 0 then
            subSeq
        else
            makeSeqLevel (computeVariants subSeq) (currentLevel - 1)
    makeSeqLevel [[]] elLen
(*
let makeCharList (characters: char list) elLen =
    let computeVariants (st: char list list) =
        characters |> List.map (fun ch -> List.map (fun s -> ch :: s) st) |> List.concat
        
    let rec makeSeqLevel (subSeq: char list list) currentLevel = 
        if currentLevel = 0 then
            subSeq
        else
            makeSeqLevel (computeVariants subSeq) (currentLevel - 1)
    makeSeqLevel [[]] elLen
*)

[<EntryPoint>]
let main argv = 
    let of3 = makeCharSeq AllSymbols 8 |> Seq.map System.String.Concat
    printfn "%A" of3

    Console.ReadKey() |> ignore
    0