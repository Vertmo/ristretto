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
  | ILOAD of int
  | FLOAD of int
  | ALOAD of int
  | ALOAD_0
  | ISTORE of int
  | FSTORE of int
  | ASTORE of int
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
  | LDC i -> [18; i]
  | ILOAD i -> [21; i]
  | FLOAD i -> [23; i]
  | ALOAD i -> [25; i]
  | ALOAD_0 -> [42]
  | ISTORE i -> [54; i]
  | FSTORE i -> [56; i]
  | ASTORE i -> [58; i]
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
