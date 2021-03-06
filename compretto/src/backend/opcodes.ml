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
  | LDC_W of int
  | LDC2_W of int
  | ILOAD of int
  | LLOAD of int
  | FLOAD of int
  | DLOAD of int
  | ALOAD of int
  | ALOAD_0
  | ISTORE of int
  | LSTORE of int
  | FSTORE of int
  | DSTORE of int
  | ASTORE of int
  | POP
  | IADD
  | LADD
  | FADD
  | DADD
  | ISUB
  | LSUB
  | FSUB
  | DSUB
  | IMUL
  | LMUL
  | FMUL
  | DMUL
  | IDIV
  | LDIV
  | FDIV
  | DDIV
  | INEG
  | LNEG
  | FNEG
  | DNEG
  | IAND
  | IOR
  | LCMP
  | FCMPG
  | DCMPG
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
  | JSR of int
  | RET of int
  | IRETURN
  | LRETURN
  | FRETURN
  | DRETURN
  | ARETURN
  | RETURN
  | GETSTATIC of int
  | PUTSTATIC of int
  | GETFIELD of int
  | PUTFIELD of int
  | INVOKEVIRTUAL of int
  | INVOKESPECIAL of int
  | INVOKESTATIC of int
  | WIDE of opcode

(** Java instruction number *)
let rec code oc = match oc with
  | ICONST_0 -> [3]
  | ICONST_1 -> [4]
  | LDC i -> [18; i]
  | LDC_W i -> 19::(u2_of_int i)
  | LDC2_W i -> 20::(u2_of_int i)
  | ILOAD i -> [21; i]
  | LLOAD i -> [22; i]
  | FLOAD i -> [23; i]
  | DLOAD i -> [24; i]
  | ALOAD i -> [25; i]
  | ALOAD_0 -> [42]
  | ISTORE i -> [54; i]
  | LSTORE i -> [55; i]
  | FSTORE i -> [56; i]
  | DSTORE i -> [57; i]
  | ASTORE i -> [58; i]
  | POP -> [87]
  | IADD -> [96]
  | LADD -> [97]
  | FADD -> [98]
  | DADD -> [99]
  | ISUB -> [100]
  | LSUB -> [101]
  | FSUB -> [102]
  | DSUB -> [103]
  | IMUL -> [104]
  | LMUL -> [105]
  | FMUL -> [106]
  | DMUL -> [107]
  | IDIV -> [108]
  | LDIV -> [109]
  | FDIV -> [110]
  | DDIV -> [111]
  | INEG -> [116]
  | LNEG -> [117]
  | FNEG -> [118]
  | DNEG -> [119]
  | IAND -> [126]
  | IOR -> [128]
  | LCMP -> [148]
  | FCMPG -> [150]
  | DCMPG -> [152]
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
  | JSR i -> 168::(u2_of_int i)
  | RET i -> [169; i]
  | IRETURN -> [172]
  | LRETURN -> [173]
  | FRETURN -> [174]
  | DRETURN -> [175]
  | ARETURN -> [176]
  | RETURN -> [177]
  | GETSTATIC i -> 178::(u2_of_int i)
  | PUTSTATIC i -> 179::(u2_of_int i)
  | GETFIELD i -> 180::(u2_of_int i)
  | PUTFIELD i -> 181::(u2_of_int i)
  | INVOKEVIRTUAL i -> 182::(u2_of_int i)
  | INVOKESPECIAL i -> 183::(u2_of_int i)
  | INVOKESTATIC i -> 184::(u2_of_int i)
  | WIDE oc -> let coc = (code oc) in match coc with
    | [co; index] -> 196::co::(u2_of_int index)
    | _ -> raise (Failure "Unexpected opcode for wide instruction")

(** Number of bytes of a segment of bytecode *)
let bytecode_length opcs = (List.length (List.concat (List.map code opcs)))
