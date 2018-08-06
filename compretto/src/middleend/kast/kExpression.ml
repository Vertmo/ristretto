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

(** k-expression type *)
type kexpr = KInt of int | KFloat of float | KString of string | KBool of bool
           | KEVar of string * Types.types (** Variable identifier. Contains type of the variable *)
           | KCall of string * kexpr list * Types.types (** Call to a function or primitive. Contains the return type *)

let rec get_type kexpr = match kexpr with
  | KInt _ -> Int | KFloat _ -> Float | KString _ -> String | KBool _ -> Bool
  | KEVar (_, t) -> t
  | KCall (_, _, t) -> t
