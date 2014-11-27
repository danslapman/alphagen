open Symbols
open System

let checkCharDoubling factor (st: char seq) =
    let freq = Seq.fold (fun (m: Map<char, int>) i -> if m.ContainsKey(i) then m.Add (i, m.Item(i) + 1) else m.Add(i, 1)) Map.empty st
    Map.forall (fun k v -> v <= factor) freq

let makeCharSeq (characters: char seq) elLen  =
    let computeVariants (st: char seq seq) =
        //characters |> Seq.map (fun ch -> Seq.map (fun s -> Seq.append (Seq.singleton ch) s) st) |> Seq.concat
        seq {
            for ch in characters do
                for sqns in st do
                    yield Seq.append (Seq.singleton ch) sqns
        }
        
    let rec makeSeqLevel (subSeq: char seq seq) currentLevel = 
        if currentLevel = 0 then
            subSeq
        else
            makeSeqLevel (computeVariants subSeq) (currentLevel - 1)
    makeSeqLevel [[]] elLen |> Seq.filter (checkCharDoubling 3)

[<EntryPoint>]
let main argv = 
    let of3 = makeCharSeq AllSymbols 10 |> Seq.map System.String.Concat
    for l in of3 do
        printfn "%s" l

    Console.ReadKey() |> ignore
    0