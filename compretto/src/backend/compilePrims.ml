(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

(** Compile the language primitives, defined in primitives.ml *)

open Types
open Primitives
open Opcodes

exception UnexpectedTypeForPrim of string

let compile_prim s t = match s with
  | "+" -> (match t with | Int -> [IADD] | Float -> [FADD] | _ -> raise (UnexpectedTypeForPrim s))
  | "-" -> (match t with | Int -> [ISUB] | Float -> [FSUB] | _ -> raise (UnexpectedTypeForPrim s))
  | "*" -> (match t with | Int -> [IMUL] | Float -> [FMUL] | _ -> raise (UnexpectedTypeForPrim s))
  | "/" -> (match t with | Int -> [IDIV] | Float -> [FDIV] | _ -> raise (UnexpectedTypeForPrim s))
  | "neg" -> (match t with | Int -> [INEG] | Float -> [FNEG] | _ -> raise (UnexpectedTypeForPrim s))
  | _ -> raise (Failure ("Compilation of primitive "^s^" not implemented"))
