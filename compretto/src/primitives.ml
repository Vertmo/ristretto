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

type op = {
  symbol: string;
  inputTypes: types list;
  outputType: output;
}

let add = {
  symbol = "+";
  inputTypes = [Int; Float; String];
  outputType = Same;
}

let sub = {
  symbol = "-";
  inputTypes = [Int; Float];
  outputType = Same;
}

let times = {
  symbol = "*";
  inputTypes = [Int; Float];
  outputType = Same;
}

let div = {
  symbol = "/";
  inputTypes = [Int; Float];
  outputType = Same;
}

let neg = {
  symbol = "-";
  inputTypes = [Int; Float];
  outputType = Same;
}

let eqeq = {
  symbol = "==";
  inputTypes = [Int; Float; String; Bool];
  outputType = Different Bool;
}

let neq = {
  symbol = "!=";
  inputTypes = [Int; Float; String; Bool];
  outputType = Different Bool;
}

let less = {
  symbol = "<";
  inputTypes = [Int; Float];
  outputType = Different Bool;
}

let lessEq = {
  symbol = "<=";
  inputTypes = [Int; Float];
  outputType = Different Bool;
}

let greater = {
  symbol = ">";
  inputTypes = [Int; Float];
  outputType = Different Bool;
}

let greaterEq = {
  symbol = ">=";
  inputTypes = [Int; Float];
  outputType = Different Bool;
}

let boolAnd = {
  symbol = "&&";
  inputTypes = [Bool];
  outputType = Same;
}

let boolOr = {
  symbol = "||";
  inputTypes = [Bool];
  outputType = Same;
}

let boolNot = {
  symbol = "not";
  inputTypes = [Bool];
  outputType = Same;
}
