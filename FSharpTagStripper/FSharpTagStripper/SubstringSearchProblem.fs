module SubstringSearchProblem

open System
open System.IO
open System.Collections.Generic

let solve_problem (input:TextReader) (output:TextWriter) =
    
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
    
    let rec find_match (index:int) (word:string) (text:string) (table:int[]) =

        let test_word n (word:string) (text:string) (table:int[]) =
//            let skipped = Seq.skip n text 
//            let bools   = Seq.zip word skipped |> Seq.map (fun p -> (fst p) = (snd p)) 

            let wordlen = word.Length
            let searchtext = Seq.skip n text |> Seq.truncate wordlen 

            if Seq.length searchtext < wordlen then false
            
            Seq.compareWith (fun a b -> if a = b then 0 else 1) word searchtext 


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
        let table = compute_table word
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