module SearchSubstring2 

open System

let get_proper_prefixes L (word:seq<char>) =
    seq {for I = L-1 downto 1 do yield Seq.take I word} |> Seq.toList

let get_proper_suffixes L (word:seq<char>) =
    seq {for I = 1 to L-1 do yield Seq.skip I word} |> Seq.toList

let equal seq1 seq2 =
    Seq.compareWith (fun x y -> if x = y then 0 else 1) seq1 seq2

let first_match pairs =
    let attempt = Seq.tryFind (fun (s,p) -> ((equal s p) = 0)) pairs
    match attempt with
    | None -> 0
    | Some(pair) -> fst pair |> Seq.length

let get_partial_match_value L (word:seq<char>) =
    let suffixes = Seq.take L word |> get_proper_suffixes L
    let prefixes = Seq.take L word |> get_proper_prefixes L

    match (Seq.isEmpty suffixes, Seq.isEmpty prefixes) with
    | (true, true) -> 0
    | (_,_) -> Seq.zip suffixes prefixes |> first_match
    
