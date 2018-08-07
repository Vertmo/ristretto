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
  | IADD
  | FADD
  | ISUB
  | FSUB
  | IMUL
  | FMUL
  | IDIV
  | FDIV

(** Java instruction number *)
let code oc = match oc with
  | IADD -> 96
  | FADD -> 98
  | ISUB -> 100
  | FSUB -> 102
  | IMUL -> 104
  | FMUL -> 106
  | IDIV -> 108
  | FDIV -> 110
