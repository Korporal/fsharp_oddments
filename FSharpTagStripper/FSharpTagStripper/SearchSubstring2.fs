module SearchSubstring2 

open System

let get_proper_prefixes L (word:seq<char>) =
    seq {for I = L-1 downto 1 do yield Seq.take I word} 

let get_proper_suffixes L (word:seq<char>) =
    seq {for I = 1 to L-1 do yield Seq.skip I word} 

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
    
let generate_table (word:string) =
    let L = word.Length 
    seq {for I = 1 to L do yield Seq.take I word |> get_partial_match_value I} |> Seq.toArray

let compute_table (word:string) =
    let prev_longest = 0
    let index = 1
    let table = Array.create word.Length 0
    
    let rec inner_computation index prev (table:int[]) =
        if index >= word.Length then
           table
        else
            if word.[index] = word.[prev] then
               table.[index] <- prev+1
               inner_computation (index+1) (prev+1) table
            else
                if prev = 0 then
                   table.[index] <- 0
                   inner_computation (index+1) prev table
                else
                   inner_computation index table.[prev-1] table
    
    inner_computation index prev_longest table

