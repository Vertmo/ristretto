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

type binOp = {
  symbol: string;
  inputTypes: types list;
  outputType: output;
}

let add = {
  symbol = "+";
  inputTypes = [Int; Float];
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
