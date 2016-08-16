module SubstringSearchProblem

open System

let solve_problem() =
    
    // Returns a sequence of proper suffixes of the input string in descending order by length.
    let get_suffixes (text:string) =
        let L = text.Length - 1
        seq {for I = 1 to L do yield text.[I .. L]} 
    
    // Returns a sequence of proper prefixes of the input string in descending order by length.
    let get_prefixes (text:string) =
        let L = text.Length - 2
        seq {for I = L downto 0 do yield text.[0 .. I]}

    // Given a sequence of matching suffix/prefix pairs, returns the length of the longest.
    let longest_match pairs =
        match Seq.isEmpty pairs with
        | true  -> 0
        | false -> pairs |> Seq.map (fun e -> (String.length (fst e))) |> Seq.max

    // Given a string, returns the length of the longest pair of suffixes and prefixes which have same length.
    let get_partial_match_value (word:string) =
        let suffixes = get_suffixes word
        let prefixes = get_prefixes word
        Seq.zip suffixes prefixes |> Seq.filter (fun e -> (fst e) = (snd e)) |> longest_match

    let generate_table (word:string) =
        let L = word.Length - 1
        seq {for I = 0 to L do yield get_partial_match_value word.[0 .. I]} |> Seq.toArray

    let test_word n (word:string) (text:string) (table:int[]) =
        let skipped = Seq.skip n text
        let bools = Seq.zip word skipped |> Seq.map (fun p -> (fst p) = (snd p)) |> Seq.toList
        try
            let p = Seq.findIndex (fun e -> e = true) bools 
            n + table.[p]
        with
            | :? System.Collections.Generic.KeyNotFoundException -> n + 1

    let is_substring (word:string) (text:string) =
        let table = generate_table word
        test_word 0 word text
    0