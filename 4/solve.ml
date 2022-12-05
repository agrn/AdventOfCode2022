let read_lines in_channel =
  let rec aux lines =
    try aux ((String.trim (input_line in_channel)) :: lines)
    with End_of_file -> lines in
  List.rev (aux [])

let group_of_split split =
  match String.split_on_char '-' split with
  | [first; last] ->
     let first = int_of_string first and
         last = int_of_string last in
     min first last, max first last
  | _ -> failwith "Bad group."

let parse_line line =
  match String.split_on_char ',' line with
  | [g1; g2] -> group_of_split g1, group_of_split g2
  | _ -> failwith "Too many or not enough groups in the line."

let have_overlap_v1 ((f1, l1), (f2, l2)) =
  (f1 <= f2 && l1 >= l2) ||
    (f1 >= f2 && l1 <= l2)

let have_overlap_v2 ((f1, l1), (f2, l2)) =
  (f1 <= f2 && l1 >= f2) ||
    (f2 <= f1 && l2 >= f1)

let apply_overlap_count fn groups =
  List.filter fn groups
  |> List.length
  |> Printf.printf "%d\n"

let () =
  if Array.length Sys.argv <> 2 then
    failwith "No filename.";
  let groups = open_in Sys.argv.(1)
               |> read_lines
               |> List.map parse_line in
  apply_overlap_count have_overlap_v1 groups;
  apply_overlap_count have_overlap_v2 groups
