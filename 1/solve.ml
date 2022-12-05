module Storage = struct
  type t = int * int * int

  let empty = 0, 0, 0

  let push_result (o, t, h) n =
    if n > o then
      (n, o, t)
    else if n > t then
      (o, n, t)
    else if n > h then
      (o, t, n)
    else
      (o, t, h)

  let max (o, _, _) = o
  let sum (o, t, h) = o + t + h
end

let handle_line line elts current =
  if line = "" then
    Storage.push_result elts current, 0
  else
    elts, current + int_of_string line

let rec read_lines in_channel elts current =
  let line, continue = try input_line in_channel, true
                       with End_of_file -> "", false in
  let line = String.trim line in
  let elts, current = handle_line line elts current in
  if continue then
    read_lines in_channel elts current
  else
    elts

let () =
  if Array.length Sys.argv <> 2 then
    failwith "No filename.";
  let file = Sys.argv.(1) in
  let in_channel = open_in file in
  let storage = read_lines in_channel Storage.empty 0 in
  Printf.printf "%d\n%d\n" (Storage.max storage) (Storage.sum storage);
  close_in in_channel
