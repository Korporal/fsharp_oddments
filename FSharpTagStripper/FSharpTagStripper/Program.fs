// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

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
    
    let rotate n (list:list<'a>) =  
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
        rotate_list n list

    let rotated = rotate -2 ("abcdefghij" |> Seq.toList)


    0 // return an integer exit code