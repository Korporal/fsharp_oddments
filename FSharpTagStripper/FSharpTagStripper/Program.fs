// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module program =

open System

[<EntryPoint>]
let main argv = 
    
    // Motivated by an interest in simplifying an earlier C# FSM design.

    let strip_tags input = 
        let rec get_next_character (flag, text) = 
            match flag with
            | _ when (Seq.isEmpty text)    -> None 
            | _ when (Seq.head text = '<') -> get_next_character (false,  Seq.tail text) 
            | _ when (Seq.head text = '>') -> get_next_character (true,   Seq.tail text) 
            | false                        -> get_next_character (false,  Seq.tail text) 
            | true                         -> Some (Seq.head text, (true, Seq.tail text)) 
        
        Seq.unfold get_next_character (true, input) |> Seq.toArray |> String 

    let result = strip_tags "This text is HTML with an embedded<img> image tag."

    // Motivated by: https://social.msdn.microsoft.com/Forums/vstudio/en-US/11be467d-9153-4318-8d79-f300ce9ccf39/f-get-each-element-from-the-list-in-seqchoose?forum=fsharpgeneral

    let lines = [| ("A 1 100"); ("B 2 200"); ("C 3 300") |]

    let tupelize input =
        let create_tuple (text:string) =
            (Int32.Parse (text.Split ' ').[1], Int32.Parse (text.Split ' ').[2])
        Array.map create_tuple input
    
    let tuples = tupelize lines 

    // Drop every n'the element: http://ocaml.org/learn/tutorials/99problems.html

    let rec drop_every n sequence = 
        let has_at_least j sequence = Seq.truncate j sequence |> Seq.length >= j
        match has_at_least n sequence with
        | false -> sequence
        | true  -> Seq.append (Seq.take (n-1) sequence) (drop_every n (Seq.skip n sequence))
           
    let ss = drop_every 3 "123456789" |> Seq.toList   // list just makes viewing results in debug easier.
    
    // Rotate a list n places left or right: http://ocaml.org/learn/tutorials/99problems.html
    
    let rotate n (list:seq<'a>) =  
        let rec rotate_list n (list:list<'a>) =
            let take_first (list:list<'a>) = list |> List.take 1
            let skip_first (list:list<'a>) = list |> List.skip 1
            let take_last (list:list<'a>)  = List.rev list |> take_first
            let skip_last (list:list<'a>)  = List.rev list |> skip_first |> List.rev
            let append_to lista listb = List.append lista listb
            match n, list with
            | _, _  when n = 0 -> list
            | _, _  when n % list.Length = 0 -> list
            | _, _  when n > 0 -> (list |> take_first |> append_to (list |> skip_first)) |> rotate_list (n - 1) 
            | _, _  when n < 0 -> (list |> skip_last  |> append_to (list |> take_last))  |> rotate_list (n + 1) 
        rotate_list n (Seq.toList list)

    let rotated = "abcdefghij" |> rotate -2 

    // Super digit problem from: https://www.hackerrank.com/challenges/super-digit
    let tcalculate name =
        name.ToString().ToLower().ToCharArray() |> Seq.map (fun char -> Convert.ToInt32 char - 96) |> Seq.sum



    let super_digits () =
        let get_char () =
            let code = Operators.stdin.Read()
            match code with
            | -1 -> None
            | 13 -> None
            |  _ -> Some(char code, 0)

        let input = (Seq.unfold (fun _ -> get_char()) 0) |> Seq.toList

        let add_digits digits_sequence =
            // Represent two sequences as if they were one sequence
            let sequentify first_sequence second_sequence = 
                seq {yield! first_sequence; yield! second_sequence}
        
            // Return the one or two char result of adding one or two supplied chars.
            let add digits = 
                let digits_list = digits |> Seq.toList
                match digits_list.Length with
                | 1 -> digits
                // Convert each char to an int add them, then subtract 96 from result to acccount for their ASCII codes
                | _ -> (((int digits_list.Head) + (int digits_list.Tail.Head) - 96) |> string).ToCharArray() |> Array.toList

            // Takes two digits from the input adds them and then (in effect) prepends either one or two digits to 
            // the remaining digits and recursively repeats the operation and that new sequence.
            let rec take_and_add sequence = 
                let digits_taken = sequence |> Seq.truncate 2 |> Seq.toList 
                let remaining_digits = sequence |> Seq.skip 2
                match digits_taken.Length with
                | 1 -> digits_taken
                // Represent the summed digits and the remaining digits as a new sequence
                // and repeat.
                | _ -> sequentify (add digits_taken) remaining_digits |> take_and_add
        
            sequentify List.empty<char> digits_sequence |> take_and_add
        add_digits input

    //let result = super_digits()



//    let concentrate sequence =
//        let compute_digit prev_char next_char =
//            match prev_char with
//            | '0' -> next_char
//            | '1' -> match next_char with | '0' -> '1' | '1' -> '2' | '2' -> '3' | '3' -> '4' | '4' -> '5' | '5' -> '6' | '6' -> '7' | '7' -> '8' | '8' -> '9' | '9' -> '1' 
//            | '2' -> match next_char with | '0' -> '2' | '1' -> '3' | '2' -> '4' | '3' -> '5' | '4' -> '6' | '5' -> '7' | '6' -> '8' | '7' -> '9' | '8' -> '1' | '9' -> '2' 
//            | '3' -> match next_char with | '0' -> '3' | '1' -> '4' | '2' -> '5' | '3' -> '6' | '4' -> '7' | '5' -> '8' | '6' -> '9' | '7' -> '1' | '8' -> '2' | '9' -> '3' 
//            | '4' -> match next_char with | '0' -> '4' | '1' -> '5' | '2' -> '6' | '3' -> '7' | '4' -> '8' | '5' -> '9' | '6' -> '1' | '7' -> '2' | '8' -> '3' | '9' -> '4' 
//            | '5' -> match next_char with | '0' -> '5' | '1' -> '6' | '2' -> '7' | '3' -> '8' | '4' -> '9' | '5' -> '1' | '6' -> '2' | '7' -> '3' | '8' -> '4' | '9' -> '5' 
//            | '6' -> match next_char with | '0' -> '6' | '1' -> '7' | '2' -> '8' | '3' -> '9' | '4' -> '1' | '5' -> '2' | '6' -> '3' | '7' -> '4' | '8' -> '5' | '9' -> '6' 
//            | '7' -> match next_char with | '0' -> '7' | '1' -> '8' | '2' -> '9' | '3' -> '1' | '4' -> '2' | '5' -> '3' | '6' -> '4' | '7' -> '5' | '8' -> '6' | '9' -> '7' 
//            | '8' -> match next_char with | '0' -> '8' | '1' -> '9' | '2' -> '1' | '3' -> '2' | '4' -> '3' | '5' -> '4' | '6' -> '5' | '7' -> '6' | '8' -> '7' | '9' -> '8' 
//            | '9' -> match next_char with | '0' -> '9' | '1' -> '1' | '2' -> '2' | '3' -> '3' | '4' -> '4' | '5' -> '5' | '6' -> '6' | '7' -> '7' | '8' -> '8' | '9' -> '9' 
//
//        Seq.reduce (compute_digit) sequence
//
//    let test = concentrate ['8';'4';'9';'7';'3';'6';'3';'7';'4';]
//
//    let solve_problem () =
//        let get_char () =
//            let code = Operators.stdin.Read()
//            match code with
//            | 32 -> None
//            | 13 -> None
//            | -1 -> None
//            |  _ -> Some(char code, 0)
//
//        let input () 
//            = (Seq.unfold (fun _ -> get_char()) 0) 
//
//        let summation = concentrate (input())
//
//        let multiplier = input() |> Seq.toArray |> String |> Convert.ToInt32
//
//        let total = multiplier * ((int summation)-48)
//
//        let result = concentrate (total.ToString().ToCharArray())
//
//        Operators.stdout.Write(result)

    SuperdigitProblem.solve_problem()

    0 // return an integer exit code