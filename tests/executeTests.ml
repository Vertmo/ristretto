(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

open OUnit2

(** Command result to list of strings *)
let process_output_to_list2 = fun command ->
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e::!res;
    process_otl_aux() in
  try process_otl_aux ()
  with End_of_file ->
    let stat = Unix.close_process_in chan in (List.rev !res,stat)

let cmd_to_list command =
  let (l,_) = process_output_to_list2 command in l

let intTest ctxt =
  assert_equal ["=============================================";
                "52";
                "============================================="] (cmd_to_list "cd samples; java int")

let floatTest ctxt =
  assert_equal ["=============================================";
                "42.1865";
                "============================================="] (cmd_to_list "cd samples; java float")

let boolTest ctxt =
  assert_equal ["=============================================";
                "false";
                "============================================="] (cmd_to_list "cd samples; java bool")

let stringTest ctxt =
  assert_equal ["=============================================";
                "Hello There !";
                "============================================="] (cmd_to_list "cd samples; java string")

let letTest ctxt =
  assert_equal ["=============================================";
                "Hello World";
                "============================================="] (cmd_to_list "cd samples; java let")

let binOpTest ctxt =
  assert_equal ["=============================================";
                "10";
                "============================================="] (cmd_to_list "cd samples; java binOp")

let boolExprTest ctxt =
  assert_equal ["=============================================";
                "true";
                "============================================="] (cmd_to_list "cd samples; java boolExpr")

let printTest ctxt =
  assert_equal ["4";
                "51.0";
                "true";
                "Hello World";
                "=============================================";
                "42";
                "============================================="] (cmd_to_list "cd samples; java print")

let ifTest ctxt =
  assert_equal ["=============================================";
                "5";
                "============================================="] (cmd_to_list "cd samples; java if")

let functionTest ctxt =
  assert_equal ["Hello";
                "4";
                "4.5";
                "1";
                "false";
                "=============================================";
                "720";
                "============================================="] (cmd_to_list "cd samples; java function")

let suite = "execute">:::[
    "int">::intTest;
    "float">::floatTest;
    "bool">::boolTest;
    "string">::stringTest;
    "let">::letTest;
    "binOp">::binOpTest;
    "boolExpr">::boolExprTest;
    "print">::printTest;
    "if">::ifTest;
    "function">::functionTest;
  ]
