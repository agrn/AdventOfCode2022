module CSet = Set.Make (Char)

let make_set str =
  String.fold_right CSet.add str CSet.empty

let read_lines in_channel =
  let rec aux lines =
    try aux ((String.trim (input_line in_channel)) :: lines)
    with End_of_file -> lines in
  List.rev (aux [])

let priority_of_char c =
  let code = Char.code c in
  if code < 97 then
    (code - Char.code 'A') + 27
  else
    (code - Char.code 'a') + 1

let split_in_parts str =
  let length = String.length str in
  if length mod 2 <> 0 then
    failwith "Incorrect line."
  else
    let half_length = length / 2 in
    String.sub str 0 half_length, String.sub str half_length half_length

let solve_v1 lines =
  List.map split_in_parts lines
  |> List.map (fun (p1, p2) ->
    let inter = CSet.inter (make_set p1) (make_set p2) in
    CSet.fold (fun elt sum -> sum + (priority_of_char elt)) inter 0)
  |> List.fold_left (+) 0
  |> Printf.printf "%d\n"

let solve_v2 lines =
  let rec aux sum lines =
    match lines with
    | l1 :: l2 :: l3 :: rem ->
      let objs1 = make_set l1 and
          objs2 = make_set l2 and
          objs3 = make_set l3 in
      let inter = CSet.inter objs1 @@ CSet.inter objs2 objs3 in
      let priority = priority_of_char @@ CSet.min_elt inter in
      aux (sum + priority) rem
    | _ -> Printf.printf "%n\n" sum in
  aux 0 lines

let () =
  if Array.length Sys.argv <> 2 then
    failwith "No filename.";
  let lines = open_in Sys.argv.(1)
              |> read_lines in
  solve_v1 lines;
  solve_v2 lines
