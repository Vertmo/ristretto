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
type kexpr = KInt of int | KLong of int64 | KFloat of float | KDouble of float | KString of string | KBool of bool | KUnit
           | KEVar of string * types (** Variable identifier. Contains type of the variable *)
           | KCall of string * kexpr list * types (** Call to a function or primitive. Contains the return type *)
           | KIf of kexpr * kprogram * kprogram * types (** if then else with return type *)

(** k-statement type *)
and kstmt = KVoidExpr of kexpr
          | KLet of string * kexpr
          | KReturn of kexpr
          | KPrint of kexpr
          | KFunction of (string * string list * (string * types) list * kprogram * types) (** Name, args, free variables, body and type *)
          | KForeign of (string * string * string list * string * types) (** Name, fcn of class, field path inside class, method name and type *)

(* k-program (list of k-statement) *)
and kprogram = kstmt list

(** Get type of the kexpr *)
let rec get_type kexpr = match kexpr with
  | KInt _ -> Int | KLong _ -> Long | KFloat _ -> Float | KDouble _ -> Double | KString _ -> String | KBool _ -> Bool | KUnit -> Unit
  | KEVar (_, t) -> t
  | KCall (_, _, t) -> t
  | KIf (_, _, _, t) -> t

(** Print an indentation *)
let indent tab_level = for _ = 0 to (tab_level-1) do print_string "  " done

(** Print a list with values separated by commas *)
let rec print_csv_list l printFun = match l with
  | [] -> ()
  | [e] -> printFun e
  | h::q -> printFun h; print_string ", "; print_csv_list q printFun

(** Print a k-expression *)
let rec pretty_print_expr kexpr tab_level = match kexpr with
  | KInt i -> indent tab_level; printf "KInt[%d]" i
  | KLong l -> indent tab_level; printf "KLong[%s]" (Int64.to_string l)
  | KFloat f -> indent tab_level; printf "KFloat[%f]" f
  | KDouble d -> indent tab_level; printf "KDouble[%f]" d
  | KString s -> indent tab_level; printf "KString[%s]" s
  | KBool b -> indent tab_level; printf "KBool[%b]" b
  | KUnit -> indent tab_level; print_string "KUnit"
  | KEVar (ident, t) -> indent tab_level; printf "KEVar[%s](%s)" ident (string_of_type t)
  | KCall (ident, kes, t) -> indent tab_level; printf "KCall[%s](%s) {\n" ident (string_of_type t);
    List.iter (fun ke -> pretty_print_expr ke (tab_level+1); print_string "\n") kes;
    indent tab_level; print_string "}"
  | KIf (cond, th, el, t) -> indent tab_level; printf "KIf(%s) {\n" (string_of_type t);
    pretty_print_expr cond (tab_level + 1); print_newline ();
    indent tab_level; print_endline "} THEN {";
    pretty_print_program th (tab_level + 1);
    indent tab_level; print_endline "} ELSE {";
    pretty_print_program el (tab_level + 1);
    indent tab_level; print_string "}"

(** Print a k-statement *)
and pretty_print_stmt kstmt tab_level = match kstmt with
  | KVoidExpr ke -> indent tab_level; printf "KVoidExpr(%s) {\n" (string_of_type (get_type ke));
    pretty_print_expr ke (tab_level + 1); print_newline ();
    indent tab_level; print_string "}"
  | KLet (ident, ke) -> indent tab_level; printf "KLet[%s](%s) {\n" ident (string_of_type (get_type ke));
    pretty_print_expr ke (tab_level + 1); print_newline ();
    indent tab_level; print_string "}"
  | KReturn ke -> indent tab_level; printf "KReturn(%s) {\n" (string_of_type (get_type ke));
    pretty_print_expr ke (tab_level + 1); print_newline ();
    indent tab_level; print_string "}"
  | KPrint ke -> indent tab_level; printf "KPrint(%s) {\n" (string_of_type (get_type ke));
    pretty_print_expr ke (tab_level + 1); print_newline ();
    indent tab_level; print_string "}"
  | KFunction (ident, args, freeVars, body, t) -> indent tab_level; printf "KFunction[%s](%s) { " ident (string_of_type t);
    print_csv_list args print_string; print_endline " }";
    indent (tab_level+1); print_string "["; print_csv_list (List.map (fun (s, t) -> s^" : "^(string_of_type t)) freeVars) print_string; print_endline "] {";
    pretty_print_program body (tab_level + 1);
    indent tab_level; print_string "}";
  | KForeign (ident, fcn, fieldPath, methodN, t) -> indent tab_level; printf "KForeign[%s](%s) {\n" ident (string_of_type t);
    indent (tab_level + 1); printf "%s." fcn; List.iter (fun s -> printf "%s." s) fieldPath; print_endline methodN;
    indent tab_level; print_string "}"

(* Print the k-program *)
and pretty_print_program kast tab_level =
  List.iter (fun kstmt -> pretty_print_stmt kstmt tab_level; print_string "\n") kast

