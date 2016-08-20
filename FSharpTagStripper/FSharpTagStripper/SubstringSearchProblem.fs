module SubstringSearchProblem

open System
open System.IO
open System.Collections.Generic

let solve_problem (input:TextReader) (output:TextWriter) =
    

    let generate_table (word:string) =

        // Given a string, returns the length of the longest pair of suffixes and prefixes which have same length.
        let get_partial_match_value (word:string) =

            // Returns a sequence of proper suffixes of the input string in descending order by length.
            let get_proper_suffixes (text:string) =
                let L = word.Length
                seq {for I = 1 to L-1 do yield Seq.skip I word} 

            // Returns a sequence of proper prefixes of the input string in descending order by length.
            let get_proper_prefixes (word:string) =
                let L = word.Length
                seq {for I = L-2 downto 1 do yield Seq.take I word} 


            // Given a sequence of matching suffix/prefix pairs, returns the length of the longest.
            let longest_match pairs =
                let attempt = Seq.tryFind (fun (s,p) -> s = p) pairs
                match attempt with
                | None -> 0
                | Some(pair) -> fst pair |> Seq.length

            let suffixes = get_proper_suffixes word
            let prefixes = get_proper_prefixes word

            match (Seq.isEmpty suffixes, Seq.isEmpty prefixes) with
            | (true, true) -> 0
            | (_,_) -> Seq.zip suffixes prefixes |> longest_match

        let L = word.Length - 1
        seq {for I = L downto 0 do yield Seq.take I word |> get_partial_match_value } |> Seq.toArray |> Array.rev

    let rec find_match (index:int) (word:string) (text:string) (table:int[]) =

        let test_word n (word:string) (text:string) (table:int[]) =
            let skipped = Seq.skip n text 
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

    let read_int() = input.ReadLine() |> Convert.ToInt32
    let read_text() = input.ReadLine()

    let get_case() = (read_text(), read_text())
    let translate b =
        match b with
        | true  -> "YES"
        | false -> "NO"


    let count = read_int()   

    let results = seq {for i = 1 to count do yield get_case()} |> Seq.map (fun pair -> is_substring (snd pair) (fst pair)) 

    Seq.iter (fun b -> output.WriteLine (translate b)) results

//    is_substring "abababca" "bacbababaabcbab"