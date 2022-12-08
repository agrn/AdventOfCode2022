let read_lines in_channel =
  let rec aux lines =
    try aux ((String.trim @@ input_line in_channel) :: lines)
    with End_of_file -> lines in
  List.rev (aux [])

let parse_lines lines =
  let height = List.length lines in
  let width = match lines with
    | l :: _ -> String.length l
    | _ -> 0 in
  let array = Array.make (height * width) 0 in
  ignore @@ List.fold_left (fun i line ->
                String.fold_left (fun i cur ->
                    array.(i) <- (int_of_char cur) - int_of_char '0';
                    i + 1) i line) 0 lines;
  height, width, array

let index width i j =
  i * width + j

let is_visible height width array i j =
  let index = index width in
  let value = array.(index i j) in
  let rec ltr_visible j' v =
    j' = j || (value > array.(index i j') && ltr_visible (j' + v) v) in
  let rec ttb_visible i' v =
    i' = i || (value > array.(index i' j) && ttb_visible (i' + v) v) in
  ltr_visible 0 1 || ltr_visible (width - 1) (-1)
  || ttb_visible 0 1 || ttb_visible (height - 1) (-1)

let count_visible_trees height width array =
  let cnt = ref 0 in
  let is_visible = is_visible height width array in

  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if is_visible i j then
        incr cnt
    done
  done;

  Printf.printf "%d\n" !cnt

let compute_score height width array i j =
  let index = index width in
  let value = array.(index i j) in
  let rec ltr_score j' v score =
    if j' >= 1 && j' < (width - 1) && value > array.(index i j') then
      ltr_score (j' + v) v (score + 1)
    else
      score in
  let rec ttb_score i' v score =
    if i' >= 1 && i' < (height - 1) && value > array.(index i' j) then
      ttb_score (i' + v) v (score + 1)
    else
      score in
  (ltr_score (j + 1) 1 1) * (ltr_score (j - 1) (-1) 1) * (ttb_score (i + 1) 1 1) * (ttb_score (i - 1) (-1) 1)

let max_scenic_score height width array =
  let score = ref 0 in
  let compute_score = compute_score height width array in

  for i = 1 to height - 2 do
    for j = 1 to width - 2 do
      let new_score = compute_score i j in
      if new_score > !score then
        score := new_score
    done
  done;

  Printf.printf "%d\n" !score

let () =
  if Array.length Sys.argv <> 2 then
    failwith "No filename.";
  let height, width, array = open_in Sys.argv.(1)
                             |> read_lines
                             |> parse_lines in
  count_visible_trees height width array;
  max_scenic_score height width array
