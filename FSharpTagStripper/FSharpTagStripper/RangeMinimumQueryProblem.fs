
module RangeMinimumQueryProblem
open System


   
let solve_problem() =        
    let inseq terminators = 
        let get_char chars =
            let code = Operators.stdin.Read()
            match chars with
            | _    when Seq.exists (fun x -> x = code) chars -> None
            | _ -> Some(char code, 0)
        (Seq.unfold (fun _ -> get_char terminators) 0) 

    let min s e list =
        let range = list |> Seq.skip s |> Seq.take (e+1-s) |> Seq.toList
        Seq.min range
        
    let read_integer () = (inseq [13;32;-1]) |> Seq.toArray |> String |> Convert.ToInt32
    let read_integer_list n = seq {for i in 1..n do yield read_integer()} |> Seq.toList
    let read_integer_pair () = (read_integer(), read_integer())
    let array_length = read_integer ()
    let num_queries  = read_integer ()
         
    let values = read_integer_list array_length 

    let read_ranges n = seq {for i in 1..n do yield read_integer_pair()} |> Seq.toList

    Seq.iter (fun p -> Operators.stdout.WriteLine(min (fst p) (snd p) values)) (read_ranges num_queries)