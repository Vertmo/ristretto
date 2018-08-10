(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

(** Primitives of the language *)

open Types

type binaryOp = Add | Sub | Mult | Div | Eqeq | Neq | Less | LessEq | Greater | GreaterEq | And | Or

type unaryOp = Neg | Not

let un_symbol u = match u with
  | Neg -> "neg" | Not -> "not"

let bin_symbol b = match b with
  | Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
  | Eqeq -> "==" | Neq -> "!=" | Less -> "<" | LessEq -> "<=" | Greater -> ">" | GreaterEq -> ">="
  | And -> "&&" | Or -> "||"

let un_input_types u = match u with
  | Neg -> [Int; Float] | Not -> [Bool]

let un_output_type u t = match u with
  | Neg -> t | Not -> t

let nums = [Int; Float]

let bin_input_types b = match b with
  | Add -> nums | Sub -> nums | Mult -> nums | Div -> nums
  | Eqeq -> [Int; Float; Bool] | Neq -> [Int; Float; Bool]
  | Less -> nums | LessEq -> nums | Greater -> nums | GreaterEq -> nums
  | And -> [Bool] | Or -> [Bool]

let bin_output_type b t1 t2 = match b with
  | Add -> t1 | Sub -> t1 | Mult -> t1 | Div -> t1
  | Eqeq -> Bool | Neq -> Bool
  | Less -> Bool | LessEq -> Bool | Greater -> Bool | GreaterEq -> Bool
  | And -> t1 | Or -> t1

let all_prims_symbols = ["+";"-";"*";"/";"==";"!=";"<";"<=";">";">=";"&&";"||";"neg";"not"]
