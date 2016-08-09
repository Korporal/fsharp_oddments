﻿module ListHelpers

    let rotate n list =  
        let rec rotate_list n list =
            let take_first list = list |> List.take 1
            let skip_first list = list |> List.skip 1
            let take_last  list = List.rev list |> take_first
            let skip_last  list = List.rev list |> skip_first |> List.rev
            let append_to lista listb = List.append lista listb
            match n, list with
            | _, [] -> []
            | _, [ _ ] -> list
            | _, _  when n = 0 -> list
            | _, _  when n % List.length list = 0 -> list
            | _, _  when n > 0 -> (list |> take_first |> append_to (list |> skip_first)) |> rotate_list (n - 1) 
            | _, _  when n < 0 -> (list |> skip_last  |> append_to (list |> take_last))  |> rotate_list (n + 1) 
        rotate_list n (Seq.toList list)