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
open Primitives
open Statement
open Expression

let intTest ctxt =
  let ast = lexAndParse (open_in "samples/int.ris") in
  assert_equal (VoidExpr (Int 1)) (List.nth ast 0);
  assert_equal (VoidExpr (Int (max_int))) (List.nth ast 3);
  assert_equal (VoidExpr (Int (-34))) (List.nth ast 4)

let floatTest ctxt =
  let ast = lexAndParse (open_in "samples/float.ris") in
  assert_equal (VoidExpr (Float 2.25)) (List.nth ast 0);
  assert_equal (VoidExpr (Float 42.1865)) (List.nth ast 3)

let boolTest ctxt =
  let ast = lexAndParse (open_in "samples/bool.ris") in
  assert_equal (VoidExpr (Bool true)) (List.nth ast 0)

let stringTest ctxt =
  let ast = lexAndParse (open_in "samples/string.ris") in
  assert_equal (VoidExpr (String "")) (List.nth ast 0);
  assert_equal (VoidExpr (String "Hello There !")) (List.nth ast 2)

let letTest ctxt =
  let ast = lexAndParse (open_in "samples/let.ris") in
  assert_equal (Let ("one",(Int 1))) (List.nth ast 0);
  assert_equal (VoidExpr (VarCall "hello")) (List.nth ast 3)

let binOpTest ctxt =
  let ast = lexAndParse (open_in "samples/binOp.ris") in
  assert_equal (VoidExpr (BinOp (Add, (Int 1), (Int 2)))) (List.nth ast 0);
  assert_equal (VoidExpr (BinOp (Add, (Int 5), (BinOp (Div, (Int 15), (Int 5)))))) (List.nth ast 4);
  assert_equal (VoidExpr (BinOp (Div, (BinOp (Add, (Int 5), (Int 15))), (Int 5)))) (List.nth ast 5)

let boolExprTest ctxt =
  let ast = lexAndParse (open_in "samples/boolExpr.ris") in
  assert_equal (VoidExpr (BinOp (Less, (Int 1), (Int 3)))) (List.nth ast 0);
  assert_equal (VoidExpr (BinOp (Eqeq,
                                 (Bool true),
                                 (BinOp (Less, (BinOp (Add, (Int 4), (Int 5))), (Int 2)))))) (List.nth ast 3);
  assert_equal (VoidExpr (BinOp (Or, (Bool true), (Bool false)))) (List.nth ast 4)

let suite = "lexAndParse">:::[
  "int">::intTest;
  "float">::floatTest;
  "bool">::boolTest;
  "string">::stringTest;
  "let">::letTest;
  "binOp">::binOpTest;
  "boolExpr">::boolExprTest;
]
