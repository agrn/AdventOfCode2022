module SMap = Map.Make (String)

let read_lines in_channel =
  let rec aux lines =
    try aux ((String.trim @@ input_line in_channel) :: lines)
    with End_of_file -> lines in
  List.rev (aux [])

type cd_action =
  | Root
  | Previous
  | Directory of string

type ls_result =
  | Dir of string
  | File of string * int

type line_contents =
  | Cd of cd_action
  | Ls
  | Out of ls_result

let string_starts_with_or_extract sw str tfn ffn =
  if String.starts_with ~prefix:sw str then
    let swl = String.length sw in
    tfn (String.sub str swl (String.length str - swl))
  else
    ffn str

let cd_action_of_string = function
  | "/" -> Root
  | ".." -> Previous
  | d -> Directory d

let ls_result_of_string str =
  string_starts_with_or_extract "dir " str
    (fun str -> Dir str)
    (fun str -> match String.split_on_char ' ' str with
                | [size; name] ->
                   let size = int_of_string size in
                   File (name, size)
                | _ -> failwith "Invalid line.")

let parse_line line =
  string_starts_with_or_extract "$ cd " line
    (fun str -> Cd (cd_action_of_string str))
    (fun str ->
      if str = "$ ls" then
        Ls
      else
        Out (ls_result_of_string str))

let dir_of_string path =
  String.concat "/" (List.rev path)

let handle_directory size =
  let rec aux map = function
    | [] -> map
    | (topmost :: rem) as dir ->
       let dir = dir_of_string dir in
       let cursize = try SMap.find dir map
                     with Not_found -> 0 in
       let map = SMap.add dir (cursize + size) map in
       aux map rem in
  aux

let enumerate_directories term =
  List.map parse_line term
  |> List.fold_left (fun (res, current_dir) command ->
         match command, current_dir with
         | Cd Root, _ -> res, [""]
         | Cd Previous, [""] -> res, [""]
         | Cd Previous, [] -> res, [""]
         | Cd Previous, _ :: dir -> res, dir
         | Cd (Directory s), dir -> res, s :: dir
         | Ls, dir -> res, dir
         | Out (Dir _), dir -> res, dir
         | Out (File (n, s)), dir -> handle_directory s res dir, dir)
       (SMap.empty, [])
  |> fst

let solve_v1 directories =
  SMap.fold (fun _ size sum ->
      if size <= 100000 then
        sum + size
      else
        sum) directories 0
  |> Printf.printf "%d\n"

let solve_v2 directories =
  let capacity = 70000000 and
      required_for_update = 30000000 and
      total_size = SMap.find "" directories in
  let required = required_for_update - (capacity - total_size) in
  SMap.fold (fun dir size final_sz ->
      if size >= required && (final_sz > size || final_sz = 0) then
        size
      else
        final_sz) directories 0
  |> Printf.printf "%d\n"

let () =
  if Array.length Sys.argv <> 2 then
    failwith "No filename.";
  let directories = open_in Sys.argv.(1)
                    |> read_lines
                    |> enumerate_directories in
  solve_v1 directories;
  solve_v2 directories
