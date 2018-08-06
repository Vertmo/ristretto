(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

open Types

type output = Same | Different of types

type binaryOp = Add | Sub | Mult | Div | Eqeq | Neq | Less | LessEq | Greater | GreaterEq | And | Or

type unaryOp = Neg | Not

let unSymbol u = match u with
  | Neg -> "-" | Not -> "not"

let binSymbol b = match b with
  | Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
  | Eqeq -> "==" | Neq -> "!=" | Less -> "<" | LessEq -> "<=" | Greater -> ">" | GreaterEq -> ">="
  | And -> "&&" | Or -> "&&"

let unInputTypes u = match u with
  | Neg -> [Int; Float] | Not -> [Bool]

let unOutputType u = match u with
  | Neg -> Same | Not -> Same

let nums = [Int; Float]

let binInputTypes b = match b with
  | Add -> nums | Sub -> nums | Mult -> nums | Div -> nums
  | Eqeq -> [Int; Float; String; Bool] | Neq -> [Int; Float; String; Bool]
  | Less -> nums | LessEq -> nums | Greater -> nums | GreaterEq -> nums
  | And -> [Bool] | Or -> [Bool]

let dBool = Different Bool

let binOutputType b = match b with
  | Add -> Same | Sub -> Same | Mult -> Same | Div -> Same
  | Eqeq -> dBool | Neq -> dBool
  | Less -> dBool | LessEq -> dBool | Greater -> dBool | GreaterEq -> dBool
  | And -> Same | Or -> Same
