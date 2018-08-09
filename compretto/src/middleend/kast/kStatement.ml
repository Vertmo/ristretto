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
open KExpression

(** k-statement module *)

(** k-statement type *)
type kstmt = KVoidExpr of kexpr
           | KLet of string * kexpr
           | KReturn of kexpr

(** Print a kstmt *)
let rec pretty_print kstmt tab_level = match kstmt with
  | KVoidExpr ke -> indent tab_level; printf "KVoidExpr(%s) {\n" (to_string (get_type ke));
    KExpression.pretty_print ke (tab_level + 1); print_newline ();
    indent tab_level; print_string "}"
  | KLet (ident, ke) -> indent tab_level; printf "KLet[%s](%s) {\n" ident (to_string (get_type ke));
    KExpression.pretty_print ke (tab_level + 1); print_newline ();
    indent tab_level; print_string "}"
  | KReturn ke -> indent tab_level; printf "KReturn(%s) {\n" (to_string (get_type ke));
    KExpression.pretty_print ke (tab_level + 1); print_newline ();
    indent tab_level; print_string "}"

and pretty_print_program kast tab_level =
  List.iter (fun kstmt -> pretty_print kstmt tab_level; print_string "\n") kast
