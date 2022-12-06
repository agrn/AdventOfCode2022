let find_redundancy window str =
  if String.length str < window then
    failwith "No start found."
  else
    let rec aux pos =
      if pos = 0 then
        None
      else
        let substr = String.sub str pos (window - pos) in
        let char = str.[pos - 1] in
        if String.contains substr char then
          Some (pos, String.sub str pos (String.length str - pos))
        else
          aux (pos - 1) in
    aux (window - 1)

let rec find_start ?(i = 0) nr str =
  match find_redundancy nr str with
  | None -> Printf.printf "%d\n" (i + nr)
  | Some (pos, substr) -> find_start ~i:(i + pos) nr substr

let () =
  if Array.length Sys.argv <> 2 then
    failwith "No filename.";
  let line = open_in Sys.argv.(1)
             |> input_line
             |> String.trim in
  find_start 4 line;
  find_start 14 line
