module HtmlHelpers

    open System

    let strip_tags input = 
        let rec get_next_character (flag, text) = 
            match flag with
            | _ when (Seq.isEmpty text)    -> None 
            | _ when (Seq.head text = '<') -> get_next_character (false,  Seq.tail text) 
            | _ when (Seq.head text = '>') -> get_next_character (true,   Seq.tail text) 
            | false                        -> get_next_character (false,  Seq.tail text) 
            | true                         -> Some (Seq.head text, (true, Seq.tail text)) 
        
        Seq.unfold get_next_character (true, input) |> Seq.toArray |> String 
