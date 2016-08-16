module SubstringSearchProblem

open System

let solve_problem() =
    
    // Returns a sequence of proper suffixes of the input string in descending order by length.
    let get_suffixes (text:string) =
        let J = text.Length - 1
        seq {for I = 1 to J do yield text.[I .. J]} 
    
    // Returns a sequence of proper prefixes of the input string in descending order by length.
    let get_prefixes (text:string) =
        let J = text.Length - 2
        seq {for I = J downto 0 do yield text.[0 .. I]}

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
    0

