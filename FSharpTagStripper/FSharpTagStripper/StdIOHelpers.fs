﻿
module StdIOHelpers

open System

    /// <summary>
    /// Represents stdin as an infinite sequence of ints.
    /// </summary>
    /// <remarks>
    /// This function represents a sequence of ints that are read from 'stdin'. The sequence terminates whenever a value is read that is equal
    /// to any of the values in the 'terminators' argument. If stdin is mapped to the console then you can passs 13 as the 'enter' key code so that the
    /// sequence terminates when the user presses 'enter'. If stdin is mapped to a file then you can use -1 as the code that corresponds
    /// to the end of file code.
    /// </remarks> 
    /// <param name="chars">A list (which may be empty) of termination codes, any one of which will cause the sequence to end and excludes the termination code.</param>
    let inseq terminators = 
        let get_char chars =
            let code = Operators.stdin.Read()
            match chars with
            | _    when Seq.exists (fun x -> x = code) chars -> None
            | _ -> Some(char code, 0)

        (Seq.unfold (fun _ -> get_char terminators) 0) 