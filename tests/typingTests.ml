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
open Types

let intTest ctxt =
  let ast = lexAndParse (open_in "samples/int.ris") in
  let (VoidExpr e) = (List.nth ast 0) in
  assert_equal Int (check_type e [])

let floatTest ctxt =
  let ast = lexAndParse (open_in "samples/float.ris") in
  let (VoidExpr e) = (List.nth ast 0) in
  assert_equal Float (check_type e [])

let boolTest ctxt =
  let ast = lexAndParse (open_in "samples/bool.ris") in
  let (VoidExpr e) = (List.nth ast 0) in
  assert_equal Bool (check_type e [])

let stringTest ctxt =
  let ast = lexAndParse (open_in "samples/string.ris") in
  let (VoidExpr e) = (List.nth ast 0) in
  assert_equal String (check_type e [])

let letTest ctxt =
  let ast = lexAndParse (open_in "samples/let.ris") in
  let (VoidExpr e) = (List.nth ast 3) in
  assert_equal String (check_type e [("hello", String)])

let suite = "typing">:::[
  "int">::intTest;
  "float">::floatTest;
  "bool">::boolTest;
  "string">::stringTest;
  "let">::letTest;
]
