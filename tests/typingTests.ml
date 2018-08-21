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
open Ast
open Types
open TypeChecking

let intTest ctxt =
  let ast = lexAndParse (open_in "samples/int.ris") in
  let (VoidExpr e) = (List.nth ast 0) in
  assert_equal Int (check_expr_types e [])

let floatTest ctxt =
  let ast = lexAndParse (open_in "samples/float.ris") in
  let (VoidExpr e) = (List.nth ast 0) in
  assert_equal Float (check_expr_types e [])

let boolTest ctxt =
  let ast = lexAndParse (open_in "samples/bool.ris") in
  let (VoidExpr e) = (List.nth ast 0) in
  assert_equal Bool (check_expr_types e [])

let stringTest ctxt =
  let ast = lexAndParse (open_in "samples/string.ris") in
  let (VoidExpr e) = (List.nth ast 0) in
  assert_equal String (check_expr_types e [])

let letTest ctxt =
  let ast = lexAndParse (open_in "samples/let.ris") in
  let (Return e) = (List.nth ast 3) in
  assert_equal String (check_expr_types e [("hello", String)])

let binOpTest ctxt =
  let ast = lexAndParse (open_in "samples/binOp.ris") in
  let (VoidExpr e) = (List.nth ast 5) in
  assert_equal Float (check_expr_types e []);
  let (VoidExpr e) = (List.nth ast 3) in
  assert_equal Int (check_expr_types e [("x", Int)])

let boolExprTest ctxt =
  let ast = lexAndParse (open_in "samples/boolExpr.ris") in
  let (VoidExpr e) = (List.nth ast 2) in assert_equal Bool (check_expr_types e []);
  let (Return e) = (List.nth ast 5) in assert_equal Bool (check_expr_types e [])

let ifTest ctxt =
  let ast = lexAndParse (open_in "samples/if.ris") in
  let (VoidExpr e) = (List.nth ast 1) in assert_equal String (check_expr_types e [("y", Int)]);
  let (Return e) = (List.nth ast 2) in assert_equal Int (check_expr_types e [("y", Int)])

let functionTest ctxt =
  let ast = lexAndParse (open_in "samples/function.ris") in
  let env = [] in
  let env = check_stmt_types (List.nth ast 0) env in (* fun ident *)
  let env = check_stmt_types (List.nth ast 11) env in (* fun isLess *)
  assert_equal (Fun ([String], String)) (snd (List.find (fun (s, _) -> s = "ident") env));
  assert_equal (Fun ([Int; Int], Bool)) (snd (List.find (fun (s, _) -> s = "isLess") env))

let suite = "typing">:::[
    "int">::intTest;
    "float">::floatTest;
    "bool">::boolTest;
    "string">::stringTest;
    "let">::letTest;
    "binOp">::binOpTest;
    "boolExpr">::boolExprTest;
    "if">::ifTest;
    "function">::functionTest;
  ]
