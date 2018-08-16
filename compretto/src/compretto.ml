(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

(** Main module, containing the main function *)

let usage = "usage: " ^ Sys.argv.(0) ^ " [-parse] [-parse-with-analysis] [-expand] <input_file>"

type step = Parse | ParseAnalysis | Expand | All

let step = ref All

let speclist = [
  ("-parse", Arg.Unit (fun () -> step := Parse), ": only parse the code");
  ("-parse-with-analysis", Arg.Unit (fun () -> step := ParseAnalysis), ": parse the code and do static analysis");
  ("-expand", Arg.Unit (fun () -> step := Expand), ": parse and expand the code");
]

let out_from_in inputName =
  let splitted = List.rev (String.split_on_char '/' inputName) in
  let in_filename = List.hd splitted in
  let without_ext = (List.hd (String.split_on_char '.' in_filename)) in
  let out_filename = without_ext^".class" in
  List.fold_left (fun a e -> e^"/"^a) "" (List.tl splitted)^out_filename

let main filename step =
  let ic = open_in filename in
  let ast = LexAndParse.lexAndParse ic in
  if step = Parse then Ast.print_program ast
  else (
    ExistingVar.check_exist_program ast [];
    TypeChecking.check_types ast;
    if step = ParseAnalysis then TypeChecking.print_program_with_types ast []
    else (
      let kast = Expand.expand_program ast [] in
      if step = Expand then Kast.pretty_print_program kast 0
      else (
        let outputName = out_from_in filename in
        Bytegen.gen_bytecode outputName kast
      )
    )
  )
  (* TypeChecking.print_program_with_types ast (\* TODO *\) *)

let _ =
  Arg.parse speclist
    (fun x -> main x !step)
    usage
  (* if Array.length Sys.argv < 2
   * then print_endline usage
   * else main Sys.argv.(1) *)
