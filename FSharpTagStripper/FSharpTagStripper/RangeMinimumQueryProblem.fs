
module RangeMinimumQueryProblem
open System


   
let solve_problem() =        

    let min s e list =
        let range = list |> Seq.skip s |> Seq.take (e+1-s) |> Seq.toList
        Seq.min range
        
    let read_integer_line() = 
        Operators.stdin.ReadLine().Trim().Split ' ' |> Seq.map (Convert.ToInt32) |> Seq.toList

    let read_pair () =
        let line = read_integer_line() 
        (line.Item 0, line.Item 1)

    let bounds = read_pair()
    let values = read_integer_line()

    let append_min (first, second) list =
        list @ Seq.min [first; second]
    

    let reduce list =
        let output = []
        let rec proces value output list =
            let prepended = value :: output
            let nextmin() = Seq.min [list |> Seq.head; list |> Seq.tail |> Seq.head ] 
            match list with
            | [] -> List.rev prepended
            | head::tail ->  proces (nextmin()) prepended (List.skip 2 list )
        proces 0 output list

    let test = reduce values;

    let read_ranges n = seq {for i in 1..n do yield read_pair()} |> Seq.toList

    Seq.iter (fun p -> Operators.stdout.WriteLine(min (fst p) (snd p) values)) (read_ranges (snd bounds))