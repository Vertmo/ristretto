(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

(** Opcodes of the JVM used by te language *)

open Utils

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
  | INEG
  | FNEG
  | IAND
  | IOR
  | FCMPG
  | IFEQ of int
  | IFNE of int
  | IFLT of int
  | IFGE of int
  | IFGT of int
  | IFLE of int
  | IF_ICMPEQ of int
  | IF_ICMPNE of int
  | IF_ICMPLT of int
  | IF_ICMPGE of int
  | IF_ICMPGT of int
  | IF_ICMPLE of int
  | GOTO of int
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
  | INEG -> [116]
  | FNEG -> [118]
  | IAND -> [126]
  | IOR -> [128]
  | FCMPG -> [150]
  | IFEQ i -> 153::(u2_of_int i)
  | IFNE i -> 154::(u2_of_int i)
  | IFLT i -> 155::(u2_of_int i)
  | IFGE i -> 156::(u2_of_int i)
  | IFGT i -> 157::(u2_of_int i)
  | IFLE i -> 158::(u2_of_int i)
  | IF_ICMPEQ i -> 159::(u2_of_int i)
  | IF_ICMPNE i -> 160::(u2_of_int i)
  | IF_ICMPLT i -> 161::(u2_of_int i)
  | IF_ICMPGE i -> 162::(u2_of_int i)
  | IF_ICMPGT i -> 163::(u2_of_int i)
  | IF_ICMPLE i -> 164::(u2_of_int i)
  | GOTO i -> 167::(u2_of_int i)
  | RETURN -> [177]
  | GETSTATIC i -> 178::(u2_of_int i)
  | INVOKEVIRTUAL i -> 182::(u2_of_int i)
  | INVOKESPECIAL i -> 183::(u2_of_int i)
