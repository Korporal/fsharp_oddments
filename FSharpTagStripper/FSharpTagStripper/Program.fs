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

    0 // return an integer exit code