module SubstringSearchProblem

open System

let solve_problem() =
    
    // Returns a sequence of proper suffixes of the input string in descending order by length.

    let generate_table (word:string) =
        // Given a string, returns the length of the longest pair of suffixes and prefixes which have same length.
        let get_partial_match_value (word:string) =
            let get_proper_suffixes (text:string) =
                let L = text.Length - 1
                seq {for I = 1 to L do yield text.[I .. L]} 
            // Returns a sequence of proper prefixes of the input string in descending order by length.
            let get_proper_prefixes (text:string) =
                let L = text.Length - 2
                seq {for I = L downto 0 do yield text.[0 .. I]}
            // Given a sequence of matching suffix/prefix pairs, returns the length of the longest.
            let longest_match pairs =
                match Seq.isEmpty pairs with
                | true  -> 0
                | false -> pairs |> Seq.map (fun e -> (String.length (fst e))) |> Seq.max
            let suffixes = get_proper_suffixes word
            let prefixes = get_proper_prefixes word
            Seq.zip suffixes prefixes |> Seq.filter (fun e -> (fst e) = (snd e)) |> longest_match
        let L = word.Length - 1
        seq {for I = 0 to L do yield get_partial_match_value word.[0 .. I]} |> Seq.toArray

    let rec find_match (index:int) (word:string) (text:string) (table:int[]) =
        let test_word n (word:string) (text:string) (table:int[]) =
            let skipped = Seq.skip n text |> Seq.toList
            let bools   = Seq.zip word skipped |> Seq.map (fun p -> (fst p) = (snd p)) 
            let matches = Seq.takeWhile (id) bools |> Seq.length
            match matches with
            | 0 -> n
            | _ when word.Length = matches -> -1
            | _ -> n + table.[matches - 1]
        match index with
        | _ when index > text.Length -> false
        | _ -> let test = test_word index word text table
               match test with
               | -1 -> true
               | _  -> find_match (test + 1) word text table

    let is_substring (word:string) (text:string) =
        let table = generate_table word
        find_match 0 word text table

    is_substring "ababaabc" "bacbababaabcbab"