(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

open Printf
open Types

(** Module for k-expressions *)

(** k-expression type *)
type kexpr = KInt of int | KFloat of float | KString of string | KBool of bool
           | KEVar of string * Types.types (** Variable identifier. Contains type of the variable *)
           | KCall of string * kexpr list * Types.types (** Call to a function or primitive. Contains the return type *)

(** Get type of the kexpr *)
let rec get_type kexpr = match kexpr with
  | KInt _ -> Int | KFloat _ -> Float | KString _ -> String | KBool _ -> Bool
  | KEVar (_, t) -> t
  | KCall (_, _, t) -> t

let indent tab_level = for _ = 0 to (tab_level-1) do print_string "  " done

(** Print a kexpr *)
let rec pretty_print kexpr tab_level = match kexpr with
  | KInt i -> indent tab_level; printf "KInt[%d]" i
  | KFloat f -> indent tab_level; printf "KFloat[%f]" f
  | KString s -> indent tab_level; printf "KString[%s]" s
  | KBool b -> indent tab_level; printf "KBool[%b]" b
  | KEVar (ident, t) -> indent tab_level; printf "KEVar[%s](%s)" ident (to_string t)
  | KCall (ident, kes, t) -> indent tab_level; printf "KCall[%s](%s) {\n" ident (to_string t);
    List.iter (fun ke -> pretty_print ke (tab_level+1); print_string "\n") kes;
    indent tab_level; print_string "}"
