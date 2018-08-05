(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

open OUnit2

(* Main test file, calling all the test suites *)

let suite =
  "tests">:::[
    LexAndParseTests.suite;
    TypingTests.suite;
  ]

let () = run_test_tt_main suite
