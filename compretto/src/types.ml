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

type types = Int | Float | String | Bool | Fun of types list * types

let allTypes = [Int; Float; String; Bool]

(** Type to string *)
let rec string_of_type t = match t with
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Bool -> "bool"
  | Fun (a, r) -> "fun "^(string_of_types a)^" -> "^(string_of_type r)

(** List of types to string *)
and string_of_types l =
  let rec s_o_t_aux l = match l with
    | [] -> ""
    | [t] -> string_of_type t
    | h::t -> (string_of_type h)^";"^(s_o_t_aux t)
  in
  "["^(s_o_t_aux l)^"]"

(** Get type from a string *)
let type_of_string s = match s with
  | "int" -> Int
  | "float" -> Float
  | "string" -> String
  | "bool" -> Bool
  | _ -> invalid_arg "type_of_string"
