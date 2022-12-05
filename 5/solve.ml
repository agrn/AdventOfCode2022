let read_lines in_channel =
  let rec aux lines =
    try aux ((input_line in_channel) :: lines)
    with End_of_file -> lines in
  List.rev (aux [])

type state =
  | StackDef
  | Moves

let handle_stack_def stacks line =
  let nr_max_elts = (String.length line + 1) / 4 in
  let rec aux l i pos =
    if i = nr_max_elts then
      l
    else
      let l =
        if line.[pos] = ' ' then
          l
        else
          (i, line.[pos]) :: l in
      aux l (i + 1) (pos + 4) in
  aux stacks 0 1

let parse_line (state, stacks, moves) line =
  match state with
  | StackDef ->
     if String.contains line '[' then
       StackDef, handle_stack_def stacks line, []
     else
       Moves, stacks, []
  | Moves ->
     match String.split_on_char ' ' line with
     | [_; nr; _; from_stack; _; to_stack] ->
        let nr = int_of_string nr and
            from_stack = int_of_string from_stack and
            to_stack = int_of_string to_stack in
        Moves, stacks, (nr, from_stack - 1, to_stack - 1) :: moves
     | _ -> Moves, stacks, moves

let mkstacks raw_stacks =
  let nr = 1 + List.fold_left (fun nr (pos, _) -> max nr pos) 0 raw_stacks in
  let stacks = Array.init nr (fun _ -> Stack.create ()) in
  List.iter (fun (pos, name) -> Stack.push name stacks.(pos)) raw_stacks;
  stacks

let move_v1 stacks (nr, from_stack, to_stack) =
  for i = 0 to nr - 1 do
    let name = Stack.pop stacks.(from_stack) in
    Stack.push name stacks.(to_stack)
  done

let move_v2 stacks (nr, from_stack, to_stack) =
  let buf = Stack.create () in
  for i = 0 to nr - 1 do
    Stack.push (Stack.pop stacks.(from_stack)) buf
  done;
  Stack.iter (fun name -> Stack.push name stacks.(to_stack)) buf

let run move_fn raw_stacks moves =
  let stacks = mkstacks raw_stacks in
  let moves = List.rev moves in
  List.iter (move_fn stacks) moves;
  Array.iter (fun stack ->
      match Stack.top_opt stack with
      | Some elt -> Printf.printf "%c" elt
      | None -> ()) stacks;
  print_endline ""

let () =
  if Array.length Sys.argv <> 2 then
    failwith "No filename.";
  let _, raw_stacks, moves = open_in Sys.argv.(1)
                             |> read_lines
                             |> List.fold_left parse_line (StackDef, [], []) in
  run move_v1 raw_stacks moves;
  run move_v2 raw_stacks moves
