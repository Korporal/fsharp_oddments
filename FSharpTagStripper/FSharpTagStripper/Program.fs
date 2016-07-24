// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    
    let rec get_next_character (flag, text) =
        if Seq.isEmpty text then
           None;
        else
            match flag, Seq.head text with
            | false, '<' -> get_next_character (true, Seq.tail text)
            | false,  _  -> Some (Seq.head text)
            | true,  '>' -> get_next_character (false, Seq.tail text)
            | true,   _  -> get_next_character (true,  Seq.tail text)

    let filtered input = Seq.unfold get_next_character (false, input)

    0 // return an integer exit code
