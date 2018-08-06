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

let unOutputType u t = match u with
  | Neg -> t | Not -> t

let nums = [Int; Float]

let binInputTypes b = match b with
  | Add -> nums | Sub -> nums | Mult -> nums | Div -> nums
  | Eqeq -> [Int; Float; String; Bool] | Neq -> [Int; Float; String; Bool]
  | Less -> nums | LessEq -> nums | Greater -> nums | GreaterEq -> nums
  | And -> [Bool] | Or -> [Bool]

let binOutputType b t1 t2 = match b with
  | Add -> t1 | Sub -> t1 | Mult -> t1 | Div -> t1
  | Eqeq -> Bool | Neq -> Bool
  | Less -> Bool | LessEq -> Bool | Greater -> Bool | GreaterEq -> Bool
  | And -> t1 | Or -> t1
