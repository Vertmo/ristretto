(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

(** Module for k-expressions *)

open Printf
open Types

(** k-expression type *)
type kexpr = KInt of int | KFloat of float | KString of string | KBool of bool
           | KEVar of string * types (** Variable identifier. Contains type of the variable *)
           | KCall of string * kexpr list * types (** Call to a function or primitive. Contains the return type *)
           | KIf of kexpr * kprogram * kprogram * types

(** k-statement type *)
and kstmt = KVoidExpr of kexpr
          | KLet of string * kexpr
          | KReturn of kexpr
          | KPrint of kexpr

(* k-program (list of k-statement) *)
and kprogram = kstmt list

(** Get type of the kexpr *)
let rec get_type kexpr = match kexpr with
  | KInt _ -> Int | KFloat _ -> Float | KString _ -> String | KBool _ -> Bool
  | KEVar (_, t) -> t
  | KCall (_, _, t) -> t
  | KIf (_, _, _, t) -> t

let indent tab_level = for _ = 0 to (tab_level-1) do print_string "  " done

(** Print a k-expression *)
let rec pretty_print_expr kexpr tab_level = match kexpr with
  | KInt i -> indent tab_level; printf "KInt[%d]" i
  | KFloat f -> indent tab_level; printf "KFloat[%f]" f
  | KString s -> indent tab_level; printf "KString[%s]" s
  | KBool b -> indent tab_level; printf "KBool[%b]" b
  | KEVar (ident, t) -> indent tab_level; printf "KEVar[%s](%s)" ident (to_string t)
  | KCall (ident, kes, t) -> indent tab_level; printf "KCall[%s](%s) {\n" ident (to_string t);
    List.iter (fun ke -> pretty_print_expr ke (tab_level+1); print_string "\n") kes;
    indent tab_level; print_string "}"
  | KIf (cond, th, el, t) -> indent tab_level; printf "KIf(%s) {\n" (to_string t);
    pretty_print_expr cond (tab_level + 1); print_newline ();
    indent tab_level; print_endline "} THEN {";
    pretty_print_program th (tab_level + 1);
    indent tab_level; print_endline "} ELSE {";
    pretty_print_program el (tab_level + 1);
    indent tab_level; print_string "}"

(** Print a k-statement *)
and pretty_print_stmt kstmt tab_level = match kstmt with
  | KVoidExpr ke -> indent tab_level; printf "KVoidExpr(%s) {\n" (to_string (get_type ke));
    pretty_print_expr ke (tab_level + 1); print_newline ();
    indent tab_level; print_string "}"
  | KLet (ident, ke) -> indent tab_level; printf "KLet[%s](%s) {\n" ident (to_string (get_type ke));
    pretty_print_expr ke (tab_level + 1); print_newline ();
    indent tab_level; print_string "}"
  | KReturn ke -> indent tab_level; printf "KReturn(%s) {\n" (to_string (get_type ke));
    pretty_print_expr ke (tab_level + 1); print_newline ();
    indent tab_level; print_string "}"
  | KPrint ke -> indent tab_level; printf "KPrint(%s) {\n" (to_string (get_type ke));
    pretty_print_expr ke (tab_level + 1); print_newline ();
    indent tab_level; print_string "}"

(* Print the k-program *)
and pretty_print_program kast tab_level =
  List.iter (fun kstmt -> pretty_print_stmt kstmt tab_level; print_string "\n") kast

