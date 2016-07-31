
module IOHelpers

open System

    let stdin_as_char_seq () = 
        let get_char () =
            let code = Operators.stdin.Read()
            match code with
            | 32 -> None
            | 13 -> None
            | -1 -> None
            |  _ -> Some(char code, 0)

        (Seq.unfold (fun _ -> get_char()) 0) 
