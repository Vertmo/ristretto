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

(** End of a comparator bytecode *)
let comp_end = [ICONST_0; GOTO 4; ICONST_1]

let compile_prim s t = match s with
  | "+" -> (match t with | Int -> [IADD] | Float -> [FADD] | _ -> raise (UnexpectedTypeForPrim s))
  | "-" -> (match t with | Int -> [ISUB] | Float -> [FSUB] | _ -> raise (UnexpectedTypeForPrim s))
  | "*" -> (match t with | Int -> [IMUL] | Float -> [FMUL] | _ -> raise (UnexpectedTypeForPrim s))
  | "/" -> (match t with | Int -> [IDIV] | Float -> [FDIV] | _ -> raise (UnexpectedTypeForPrim s))
  | "neg" -> (match t with | Int -> [INEG] | Float -> [FNEG] | _ -> raise (UnexpectedTypeForPrim s))
  | "==" -> (match t with | Int | Bool -> (IF_ICMPEQ 7)::comp_end
                          | Float -> [FCMPG; IFEQ 7]@comp_end | _ -> raise (UnexpectedTypeForPrim s))
  | "!=" -> (match t with | Int | Bool -> (IF_ICMPNE 7)::comp_end
                          | Float -> [FCMPG; IFNE 7]@comp_end | _ -> raise (UnexpectedTypeForPrim s))
  | "<" -> (match t with | Int -> (IF_ICMPLT 7)::comp_end
                         | Float -> [FCMPG; IFLT 7]@comp_end | _ -> raise (UnexpectedTypeForPrim s))
  | "<=" -> (match t with | Int -> (IF_ICMPLE 7)::comp_end
                          | Float -> [FCMPG; IFLE 7]@comp_end | _ -> raise (UnexpectedTypeForPrim s))
  | ">" -> (match t with | Int -> (IF_ICMPGT 7)::comp_end
                         | Float -> [FCMPG; IFGT 7]@comp_end | _ -> raise (UnexpectedTypeForPrim s))
  | ">=" -> (match t with | Int -> (IF_ICMPGE 7)::comp_end
                          | Float -> [FCMPG; IFGE 7]@comp_end | _ -> raise (UnexpectedTypeForPrim s))
  | "&&" -> (match t with | Bool -> [IMUL] | _ -> raise (UnexpectedTypeForPrim s))
  | "||" -> (match t with | Bool -> [IDIV] | _ -> raise (UnexpectedTypeForPrim s))
  | "not" -> (match t with | Bool -> [IDIV] | _ -> raise (UnexpectedTypeForPrim s))
  | _ -> raise (Failure ("Compilation of primitive "^s^" not implemented"))
