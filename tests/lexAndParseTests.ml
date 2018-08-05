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
open LexAndParse
open Statement
open Expression

let intTest ctxt =
  let ast = lexAndParse (open_in "samples/int.ris") in
  assert_equal (List.nth ast 0) (VoidExpr (Int 1));
  assert_equal (List.nth ast 3) (VoidExpr (Int (max_int)));
  assert_equal (List.nth ast 4) (VoidExpr (Int (-34)))

let floatTest ctxt =
  let ast = lexAndParse (open_in "samples/float.ris") in
  assert_equal (List.nth ast 0) (VoidExpr (Float 2.25));
  assert_equal (List.nth ast 3) (VoidExpr (Float 42.1865))

let boolTest ctxt =
  let ast = lexAndParse (open_in "samples/bool.ris") in
  assert_equal (List.nth ast 0) (VoidExpr (Bool true))

let stringTest ctxt =
  let ast = lexAndParse (open_in "samples/string.ris") in
  assert_equal (List.nth ast 0) (VoidExpr (String ""));
  assert_equal (List.nth ast 2) (VoidExpr (String "Hello There !"))

let letTest ctxt =
  let ast = lexAndParse (open_in "samples/let.ris") in
  assert_equal (List.nth ast 0) (Let ("one",(Int 1)));
  assert_equal (List.nth ast 3) (VoidExpr (VarCall "hello"))

let suite = "lexAndParse">:::[
  "int">::intTest;
  "float">::floatTest;
  "bool">::boolTest;
  "string">::stringTest;
  "let">::letTest;
]
