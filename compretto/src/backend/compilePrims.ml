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
  | "+" -> (match t with | Int -> [IADD] | Long -> [LADD] | Float -> [FADD] | Double -> [DADD] | _ -> raise (UnexpectedTypeForPrim s))
  | "-" -> (match t with | Int -> [ISUB] | Long -> [LSUB] | Float -> [FSUB] | Double -> [DSUB] | _ -> raise (UnexpectedTypeForPrim s))
  | "*" -> (match t with | Int -> [IMUL] | Long -> [LMUL] | Float -> [FMUL] | Double -> [DMUL] | _ -> raise (UnexpectedTypeForPrim s))
  | "/" -> (match t with | Int -> [IDIV] | Long -> [LDIV] | Float -> [FDIV] | Double -> [DDIV] | _ -> raise (UnexpectedTypeForPrim s))
  | "neg" -> (match t with | Int -> [INEG] | Long -> [LNEG] | Float -> [FNEG] | Double -> [DNEG] | _ -> raise (UnexpectedTypeForPrim s))
  | "==" -> (match t with | Int | Bool -> (IF_ICMPEQ 7)::comp_end
                          | Long -> [LCMP; IFEQ 7]@(comp_end)
                          | Float -> [FCMPG; IFEQ 7]@comp_end
                          | Double -> [DCMPG; IFEQ 7]@comp_end | _ -> raise (UnexpectedTypeForPrim s))
  | "!=" -> (match t with | Int | Bool -> (IF_ICMPNE 7)::comp_end
                          | Long -> [LCMP; IFNE 7]@(comp_end)
                          | Float -> [FCMPG; IFNE 7]@comp_end
                          | Double -> [DCMPG; IFNE 7]@comp_end| _ -> raise (UnexpectedTypeForPrim s))
  | "<" -> (match t with | Int -> (IF_ICMPLT 7)::comp_end
                         | Long -> [LCMP; IFLT 7]@(comp_end)
                         | Float -> [FCMPG; IFLT 7]@comp_end
                         | Double -> [DCMPG; IFLT 7]@comp_end| _ -> raise (UnexpectedTypeForPrim s))
  | "<=" -> (match t with | Int -> (IF_ICMPLE 7)::comp_end
                          | Long -> [LCMP; IFLE 7]@(comp_end)
                          | Float -> [FCMPG; IFLE 7]@comp_end
                          | Double -> [DCMPG; IFLE 7]@comp_end| _ -> raise (UnexpectedTypeForPrim s))
  | ">" -> (match t with | Int -> (IF_ICMPGT 7)::comp_end
                         | Long -> [LCMP; IFGT 7]@(comp_end)
                         | Float -> [FCMPG; IFGT 7]@comp_end
                         | Double -> [DCMPG; IFGT 7]@comp_end| _ -> raise (UnexpectedTypeForPrim s))
  | ">=" -> (match t with | Int -> (IF_ICMPGE 7)::comp_end
                          | Long -> [LCMP; IFGE 7]@(comp_end)
                          | Float -> [FCMPG; IFGE 7]@comp_end
                          | Double -> [DCMPG; IFGE 7]@comp_end| _ -> raise (UnexpectedTypeForPrim s))
  | "&&" -> (match t with | Bool -> [IAND] | _ -> raise (UnexpectedTypeForPrim s))
  | "||" -> (match t with | Bool -> [IOR] | _ -> raise (UnexpectedTypeForPrim s))
  | "not" -> (match t with | Bool -> [IFEQ 7]@comp_end | _ -> raise (UnexpectedTypeForPrim s))
  | _ -> raise (Failure ("Compilation of primitive "^s^" not implemented"))
