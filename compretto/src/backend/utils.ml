(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

(** Some conversion utilities *)

type u2 = int list (* of size 2 *)

(** Convert an int into two bytes *)
let u2_of_int i =
  [i asr 8; i land 255]

type u4 = int list (* of size 4 *)

let u4_of_int i =
  [i asr (3*8); (i asr (2*8)) land 255; (i asr 8) land 255; i land 255]

let u4_of_float f =
  let i = (Int32.bits_of_float f) in
  let l = List.map Int32.to_int [Int32.shift_right_logical i (3*8);
                         Int32.logand (Int32.shift_right_logical i (2*8)) (Int32.of_int 255);
                         Int32.logand (Int32.shift_right_logical i 8) (Int32.of_int 255);
                                  Int32.logand i (Int32.of_int 255)] in
  l

let print_u2 file u2 =
  List.iter (output_byte file) u2

let print_u4 file u4 =
  List.iter (output_byte file) u4

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(** Converts a string to a u1 list *)
let u1list_of_string s =
  List.map int_of_char (explode s)

let find_from_table cpTable toFind =
  (snd (List.find (fun (s, _) -> s = toFind) cpTable))
