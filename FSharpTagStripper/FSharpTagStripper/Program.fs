// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System

[<EntryPoint>]
let main argv = 
    
//    // Motivated by an interest in simplifying an earlier C# FSM design.
//
//    let strip_tags input = 
//        let rec get_next_character (flag, text) = 
//            match flag with
//            | _ when (Seq.isEmpty text)    -> None 
//            | _ when (Seq.head text = '<') -> get_next_character (false,  Seq.tail text) 
//            | _ when (Seq.head text = '>') -> get_next_character (true,   Seq.tail text) 
//            | false                        -> get_next_character (false,  Seq.tail text) 
//            | true                         -> Some (Seq.head text, (true, Seq.tail text)) 
//        
//        Seq.unfold get_next_character (true, input) |> Seq.toArray |> String 
//
//    let result = strip_tags "This text is HTML with an embedded<img> image tag."
//
//    // Motivated by: https://social.msdn.microsoft.com/Forums/vstudio/en-US/11be467d-9153-4318-8d79-f300ce9ccf39/f-get-each-element-from-the-list-in-seqchoose?forum=fsharpgeneral
//
//    let lines = [| ("A 1 100"); ("B 2 200"); ("C 3 300") |]
//
//    let tupelize input =
//        let create_tuple (text:string) =
//            (Int32.Parse (text.Split ' ').[1], Int32.Parse (text.Split ' ').[2])
//        Array.map create_tuple input
//    
//    let tuples = tupelize lines 
//
//    // Drop every n'the element: http://ocaml.org/learn/tutorials/99problems.html
//
//    let rec drop_every n sequence = 
//        let has_at_least j sequence = Seq.truncate j sequence |> Seq.length >= j
//        match has_at_least n sequence with
//        | false -> sequence
//        | true  -> Seq.append (Seq.take (n-1) sequence) (drop_every n (Seq.skip n sequence))
//           
//    let ss = drop_every 3 "123456789" |> Seq.toList   // list just makes viewing results in debug easier.
//    
//    // Rotate a list n places left or right: http://ocaml.org/learn/tutorials/99problems.html
//    
//    let rotate n (list:seq<'a>) =  
//        let rec rotate_list n (list:list<'a>) =
//            let take_first (list:list<'a>) = list |> List.take 1
//            let skip_first (list:list<'a>) = list |> List.skip 1
//            let take_last (list:list<'a>)  = List.rev list |> take_first
//            let skip_last (list:list<'a>)  = List.rev list |> skip_first |> List.rev
//            let append_to lista listb = List.append lista listb
//            match n, list with
//            | _, _  when n = 0 -> list
//            | _, _  when n % list.Length = 0 -> list
//            | _, _  when n > 0 -> (list |> take_first |> append_to (list |> skip_first)) |> rotate_list (n - 1) 
//            | _, _  when n < 0 -> (list |> skip_last  |> append_to (list |> take_last))  |> rotate_list (n + 1) 
//        rotate_list n (Seq.toList list)
//
//    let rotated = "abcdefghij" |> rotate -2 
//
//    // Super digit problem from: https://www.hackerrank.com/challenges/super-digit
//    let tcalculate name =
//        name.ToString().ToLower().ToCharArray() |> Seq.map (fun char -> Convert.ToInt32 char - 96) |> Seq.sum


//    let code = Console.ReadKey().KeyChar
//
//    code
//
//    let run_super_digit =
//        let read_line = Operators.stdin.ReadToEnd()
//        let args = read_line.Trim().Split ' ' 
//        let solve_super_digit n k =
//            let rec super_digit_string x = 
//                let sum_digits_string s = s |> string |> Seq.fold (fun t char -> Convert.ToInt64 char + t - 48L) 0L |> string // the 48 compensates for the fact that the char's numeric value is its ASCII value.
//                match x with
//                | "0" | "1" | "2" | "3" | "4" | "5"| "6"| "7" | "8" | "9" -> x
//                | _ -> sum_digits_string x |> super_digit_string
//            String.replicate k (n |> string) |> super_digit_string
//        
//        solve_super_digit args.[0] (Convert.ToInt32 args.[1]) |> Console.WriteLine
//
    //run_super_digit



    let nseq = ['1';'4';'7']//;'5';'9';'8';'7';'4';'5';'9';'7';'5';'9';'3';'4';'6';'5';'4']
    let sseq = ['0']
    let k    = 3

    //  1 2 3 4 5 s1 = 12345 
    //  5   3 4 5 s0 = 5, s1 = 345
    //  8     4 5 s0 = 8, s1 = 45
    //  1 2     5 s0 = 12, s1 = 5
    //  3       5 s0 = 3,  s1 = 5

    let sequentify first second = seq {yield! first; yield! second}

    let add (cells:list<'char>) = 
        match cells.Length with
        | 1 -> cells
        | _ -> (((int cells.Head)-48 + (int cells.Tail.Head)-48) |> string).ToCharArray() |> Array.toList

    let rec pull_and_add first second = 
        let pulled_value = sequentify first second |> Seq.truncate 2 |> Seq.toList 
        match pulled_value.Length with
        | 1 -> pulled_value
        | _ -> pull_and_add (add pulled_value) (sequentify first second |> Seq.toList |> List.skip 2)

    let solve sequence =
        let accumulator = ['0'].Tail // i.e. an empty char list
        pull_and_add accumulator sequence


        

//    let rec evaluate first second =
//        let elements first second = seq {yield! first; yield! second} |> Seq.where (fun c -> c <> '0') |> Seq.toList
//        let first_two = elements first second |> List.take 2 //|> List.toList
//
//        if first_two.Length = 1 then first_two else
//           let sum = ((int first_two.Head)-48 + (int first_two.Tail.Head)-48) |> string 
//           let back =  (List.skip 2 (elements first second))
//           let front = (sum.ToCharArray() |> Array.toList) |> List.append first
//           evaluate front back

    //evaluate sseq nseq

    let r = solve ['3';'2';'5';'3']


    

    // read each char, and sum, when done multiply by k and reduce again



    //Seq.fold (fun sum digit -> )
//
//
//    //let vv = Convert.ToString(Console.ReadKey().KeyChar)
//
//    //let k1 = Console.ReadKey()
////
////    let k2 = Console.ReadKey()
//
//    //let inp = Seq.unfold (fun s -> Some (s, Console.ReadKey().KeyChar)) ' ' |> Seq.takeWhile (fun c -> c <> '\r') |> Seq.toArray |> System.String
//
//
    0 // return an integer exit code