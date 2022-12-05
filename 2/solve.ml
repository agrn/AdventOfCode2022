type move =
  | Rock
  | Paper
  | Scissors

type outcome =
  | Victory
  | Loss
  | Draw

let move_of_char = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _ -> failwith "Invalid char."

let outcome_of_char = function
  | "X" -> Loss
  | "Y" -> Draw
  | "Z" -> Victory
  | _ -> failwith "Invalid char."

let outcome_of_moves = function
  (* Elf, me *)
  | Rock, Paper | Paper, Scissors | Scissors, Rock -> Victory
  | e, m when e = m -> Draw
  | _, _ -> Loss

let move_of_move_outcome = function
  | Rock, Victory -> Paper
  | Paper, Victory -> Scissors
  | Scissors, Victory -> Rock
  | e, Draw -> e
  | Rock, Loss -> Scissors
  | Paper, Loss -> Rock
  | Scissors, Loss -> Paper

let score_of_move = function
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3

let score_of_outcome = function
  | Victory -> 6
  | Loss -> 0
  | Draw -> 3

module type CalculatorT = sig
  type t

  val score : (move * t) -> int
  val parse_elt : string -> t
end

module CalculatorV1 = struct
  type t = move

  let score (elf, me) =
    let outcome = outcome_of_moves (elf, me) in
    score_of_move me + score_of_outcome outcome

  let parse_elt = move_of_char
end

module CalculatorV2 = struct
  type t = outcome

  let score (elf, outcome) =
    let me = move_of_move_outcome (elf, outcome) in
    score_of_move me + score_of_outcome outcome

  let parse_elt = outcome_of_char
end

let read_lines in_channel =
  let rec aux lines =
    try aux ((String.trim (input_line in_channel)) :: lines)
    with End_of_file -> lines in
  List.rev (aux [])

module Solver (C : CalculatorT) = struct
  let parse_line line =
    match String.split_on_char ' ' line with
    | [elf; second] ->
      let elf = move_of_char elf and
          second = C.parse_elt second in
      elf, second
    | _ -> failwith "Invalid line."

  let score moves =
    List.map parse_line moves
    |> List.map C.score
    |> List.fold_left (+) 0
    |> Printf.printf "%n\n"
end

module SolverV1 = Solver (CalculatorV1)
module SolverV2 = Solver (CalculatorV2)

let () =
  if Array.length Sys.argv <> 2 then
    failwith "No filename.";
  let moves = open_in Sys.argv.(1)
              |> read_lines in
  SolverV1.score moves;
  SolverV2.score moves
