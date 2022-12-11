let read_lines in_channel =
  let rec aux lines =
    try aux ((String.trim @@ input_line in_channel) :: lines)
    with End_of_file -> lines in
  List.rev (aux [])

let (--) i j =
  let rec aux n acc =
    if n < i then
      acc
    else
      aux (n - 1) (n :: acc) in
  aux j []

module IMap = Map.Make (Int)

type monkey = {
  items : int list;
  operation : int -> int;
  test : int;
  ttrue : int;
  tfalse : int
}

let empty_monkey = { items = [];
                     operation = Fun.id;
                     test = 0;
                     ttrue = 0;
                     tfalse = 0 }

type state =
  | Monkey
  | Starting
  | Operation
  | Test
  | True
  | False
  | Void

let next_state = function
  | Monkey -> Starting
  | Starting -> Operation
  | Operation -> Test
  | Test -> True
  | True -> False
  | False -> Void
  | Void -> Monkey

let rec last = function
  | [] -> failwith "List is not long enough."
  | [elt] -> elt
  | _ :: rem -> last rem

let get_num_at_end line =
  let num_ch = String.get line @@ String.length line - 1 in
  (int_of_char num_ch) - (int_of_char '0')

let parse_monkey (state, tmp, monkeys) line =
  match state with
  | Void | Monkey -> next_state state, tmp, monkeys
  | Starting ->
    let elts =
      match String.split_on_char ':' line with
      | [_; elts] ->
        String.split_on_char ',' elts
        |> List.map (fun elt -> int_of_string @@ String.trim elt)
      | _ -> failwith "Invalid line." in
    next_state state, { empty_monkey with items = elts }, monkeys
  | Operation ->
    let operation =
      let splitted = String.split_on_char '+' line in
      let splitted, basic_operation =
        if List.length splitted = 1 then
          String.split_on_char '*' line, ( * )
        else
          splitted, (+) in
      let value = String.trim @@ last splitted in
      if value = "old" then
        (fun x -> basic_operation x x)
      else
        basic_operation (int_of_string value) in
    let tmp = { tmp with operation } in
    next_state state, tmp, monkeys
  | Test ->
    let test = String.split_on_char ' ' line
               |> last
               |> int_of_string in
    let tmp = { tmp with test } in
    next_state state, tmp, monkeys
  | True ->
    let ttrue_num = get_num_at_end line in
    let tmp = { tmp with ttrue = ttrue_num } in
    next_state state, tmp, monkeys
  | False ->
    let false_num = get_num_at_end line in
    let monkey = { tmp with tfalse = false_num } in
    next_state state, empty_monkey, monkey :: monkeys

let parse_monkeys lines =
  let _, _, monkeys = List.fold_left parse_monkey (Monkey, empty_monkey, []) lines in
  List.rev monkeys

let make_items_map monkeys =
  let map, _ = List.fold_left (fun (map, i) monkey ->
                                  IMap.add i monkey.items map, i + 1) (IMap.empty, 0) monkeys in
  map

let apply_item divf tr monkey map item =
  let new_item = (monkey.operation item) / divf in
  let next_monkey = if new_item mod monkey.test = 0 then
                      monkey.ttrue
                    else
                      monkey.tfalse in
  let new_item = tr new_item in
  let next_item_list =
    match IMap.find_opt next_monkey map with
    | None -> [new_item]
    | Some l -> new_item :: l in
  IMap.add next_monkey next_item_list map

let apply_monkey apply_item monkey i usagemap objmap =
  match IMap.find_opt i objmap with
  | None -> usagemap, objmap
  | Some items ->
    let objmap = IMap.remove i objmap in
    let nr_usages =
      match IMap.find_opt i usagemap with
      | None -> List.length items
      | Some u -> u + (List.length items) in
    IMap.add i nr_usages usagemap,
    List.fold_left (apply_item monkey) objmap items

let apply_pass apply_item monkeys usagemap objmap =
  let _, usagemap, objmap =
    List.fold_left (fun (i, usagemap, objmap) monkey ->
      let usagemap, objmap = apply_monkey apply_item monkey i usagemap objmap in
      i + 1, usagemap, objmap) (0, usagemap, objmap) monkeys in
  usagemap, objmap

let solve ?(nr = 20) apply_item monkeys =
  let objmap = make_items_map monkeys in
  let usagemap = IMap.empty in
  let range = 0 -- (nr - 1) in
  let usagemap, _ = List.fold_left (fun (usagemap, objmap) _ ->
          apply_pass apply_item monkeys usagemap objmap) (usagemap, objmap) range in
  let usagemap = IMap.bindings usagemap
                 |> List.sort (fun (_, i) (_, j) -> compare j i) in
  match usagemap with
  | (m1, u1) :: (m2, u2) :: _ ->
    Printf.printf "Monkey %d with %d objects\nMonkey %d with %d objects\n" m1 u1 m2 u2;
    Printf.printf "Monkey business level: %d\n" (u1 * u2)
  | _ -> failwith "???"

let common_modulo =
  List.fold_left (fun res monkey -> res * monkey.test) 1

let () =
  if Array.length Sys.argv <> 2 then
    failwith "No filename.";
  let monkeys = open_in Sys.argv.(1)
                |> read_lines
                |> parse_monkeys in
  let modf = common_modulo monkeys in
  solve (apply_item 3 Fun.id) monkeys;
  solve ~nr:10000 (apply_item 1 (fun x -> x mod modf)) monkeys
