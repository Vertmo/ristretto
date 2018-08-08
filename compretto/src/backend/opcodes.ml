(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

type opcode =
  | ICONST_0
  | ICONST_1
  | LDC of int
  | ALOAD_0
  | POP
  | IADD
  | FADD
  | ISUB
  | FSUB
  | IMUL
  | FMUL
  | IDIV
  | FDIV
  | RETURN
  | GETSTATIC of int
  | INVOKEVIRTUAL of int
  | INVOKESPECIAL of int

(** Java instruction number *)
let code oc = match oc with
  | ICONST_0 -> [3]
  | ICONST_1 -> [4]
  | LDC i -> print_int i; print_newline (); [18; i]
  | ALOAD_0 -> [42]
  | POP -> [87]
  | IADD -> [96]
  | FADD -> [98]
  | ISUB -> [100]
  | FSUB -> [102]
  | IMUL -> [104]
  | FMUL -> [106]
  | IDIV -> [108]
  | FDIV -> [110]
  | RETURN -> [177]
  | GETSTATIC i -> 178::(Utils.u2_of_int i)
  | INVOKEVIRTUAL i -> 182::(Utils.u2_of_int i)
  | INVOKESPECIAL i -> 183::(Utils.u2_of_int i)
