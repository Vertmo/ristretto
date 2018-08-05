(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

(** Types allowed in the language *)

type types = Int | Float | String | Bool

let to_string t = match t with
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Bool -> "bool"
