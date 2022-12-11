let read_lines in_channel =
  let rec aux lines =
    try aux ((String.trim @@ input_line in_channel) :: lines)
    with End_of_file -> lines in
  List.rev (aux [])

type instruction =
  | Nop
  | Add of int

let instruction_of_string str =
  if str = "noop" then
    Nop
  else
    match String.split_on_char ' ' str with
    | [_; value] -> Add (int_of_string value)
    | _ -> failwith "Invalid line."

let execute_instruction x nr_cycles = function
  | Nop -> x, nr_cycles + 1
  | Add value -> x + value, nr_cycles + 2

let store_strength (x, nr_cycles, strength) instruction =
  let nx, nnr = execute_instruction x nr_cycles instruction in
  if (nnr - 20) mod 40 = 0 then
    nx, nnr, nx * nnr + strength
  else if (nr_cycles - 20) mod 40 <> 0 && (nnr - 20) mod 40 = 1 then
    nx, nnr, (nr_cycles + 1) * x + strength
  else
    nx, nnr, strength

let compute_total_strength instrs =
  let _, _, strength = List.fold_left store_strength (1, 1, 0) instrs in
  Printf.printf "%d\n" strength

let getch x nr_cycles =
  let col = (nr_cycles mod 40) + 1 in
  if col >= x && x + 2 >= col then
    '#'
  else
    '.'

let addch x nr_cycles buffer =
  let ch = getch x nr_cycles in
  Buffer.add_char buffer ch;
  if (Buffer.length buffer + 1) mod 41 = 0 then
    Buffer.add_char buffer '\n'

let print_sprite buffer (x, nr_cycles) instruction =
  let nx, nnr = execute_instruction x nr_cycles instruction in
  if nnr - nr_cycles = 2 then
    addch x (nr_cycles + 1) buffer;
  addch nx nnr buffer;
  nx, nnr

let show_sprite instructions =
  let buffer = Buffer.create 247 in
  addch 1 0 buffer;
  ignore @@ List.fold_left (print_sprite buffer) (1, 0) instructions;
  print_endline (Buffer.contents buffer)

let () =
  if Array.length Sys.argv <> 2 then
    failwith "No filename.";
  let instructions = open_in Sys.argv.(1)
                     |> read_lines
                     |> List.map instruction_of_string in
  compute_total_strength instructions;
  show_sprite instructions
