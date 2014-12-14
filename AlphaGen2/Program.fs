open Symbols

let checkCharDoubling factor (st: char seq) =
    let freq = Seq.fold (fun (m: Map<char, int>) i -> if m.ContainsKey(i) then m.Add (i, m.Item(i) + 1) else m.Add(i, 1)) Map.empty st
    Map.forall (fun k v -> v <= factor) freq

[<EntryPoint>]
let main argv = 
    for c0 in AllSymbols do
        for c1 in AllSymbols do
            for c2 in AllSymbols do 
                for c3 in AllSymbols do
                    for c4 in AllSymbols do
                        for c5 in AllSymbols do
                            for c6 in AllSymbols do
                                for c7 in AllSymbols do
                                    let arr = [|c0; c1; c2; c3; c4; c5; c6; c6; c7|]
                                    if checkCharDoubling 3 arr then
                                        printfn "%s" (new System.String(arr))
                                        

    printfn "%A" argv
    0