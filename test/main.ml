open OUnit2
open Search
open QCheck

(*****************************************************************)
(* Examples of how to create data structures *)
(*****************************************************************)

module Int = struct
  type t = int

  let compare x y =
    match Stdlib.compare x y with
    | x when x < 0 -> Dictionary.LT
    | 0 -> EQ
    | _ -> GT

  let to_string = string_of_int
end

(* Example: A list dictionary that maps ints to ints. *)
module D1 = ListDictionary.Make (Int) (Int)

(* Example: A set of strings, implemented with list dictionaries. *)
module S1 = DictionarySet.Make (StringKey.String) (ListDictionary.Make)

(* Example: A tree dictionary that maps case-insensitive strings to ints. *)
module D2 = TreeDictionary.Make (StringKey.CaselessString) (Int)

(* Example: A set of strings, implemented with tree dictionaries. *)
module S2 = DictionarySet.Make (StringKey.String) (TreeDictionary.Make)

(*****************************************************************)
(* Examples of how to index a directory *)
(*****************************************************************)
let data_dir_prefix = "data" ^ Filename.dir_sep
let preamble_path = data_dir_prefix ^ "preamble"

let preamble_list_idx =
  try Some (ListEngine.E.index_of_dir preamble_path) with _ -> None

let preamble_tree_idx =
  try Some (TreeEngine.E.index_of_dir preamble_path) with _ -> None

(*****************************************************************)
(* Test suite *)
(*****************************************************************)

(** Module String imitates Module Int above as an easy KeySig or ValueSig *)
module String = struct
  type t = string

  let compare x y =
    match Stdlib.compare x y with
    | x when x < 0 -> Dictionary.LT
    | 0 -> EQ
    | _ -> GT

  let to_string (s : t) : string = s
end

(** Module type TestOrNot and Module TestList, NotTestList controls whether
    certain tests apply to this implementation of dictionary maker or not *)
module type TestOrNot = sig
  type t

  val test : bool
end

module TestList = struct
  type t = bool

  let test = true
end

module NotTestList = struct
  type t = bool

  let test = false
end

(** Module Test is a functor that reduces code duplicates *)

module Test (DM : Dictionary.DictionaryMaker) (TR : TestOrNot) = struct
  module IntIntDictionary = DM (Int) (Int)
  module StrIntDictionary = DM (String) (Int)
  module IntSet = DictionarySet.Make (Int) (DM)

  let tests =
    [
      ( "to_list test II" >:: fun _ ->
        assert_equal [] IntIntDictionary.(empty |> to_list) );
      ( "to_list test SI" >:: fun _ ->
        assert_equal [] StrIntDictionary.(empty |> to_list) );
      ( "to_string test II" >:: fun _ ->
        assert_equal "" IntIntDictionary.(empty |> to_string) );
      ( "to_string test SI" >:: fun _ ->
        assert_equal "" StrIntDictionary.(empty |> to_string) );
      ( "is empty test II" >:: fun _ ->
        assert_equal true IntIntDictionary.(empty |> is_empty) );
      ( "is empty test SI" >:: fun _ ->
        assert_equal true StrIntDictionary.(empty |> is_empty) );
      ( "size test II" >:: fun _ ->
        assert_equal 0 IntIntDictionary.(empty |> size) );
      ( "size test SI" >:: fun _ ->
        assert_equal 0 StrIntDictionary.(empty |> size) );
      ( "insert test II 1" >:: fun _ ->
        assert_equal 1 IntIntDictionary.(empty |> insert 1 1 |> size) );
      ( "insert test SI 1" >:: fun _ ->
        assert_equal 1 StrIntDictionary.(empty |> insert "a" 1 |> size) );
      ( "insert test II 2" >:: fun _ ->
        assert_equal 2
          IntIntDictionary.(empty |> insert 1 1 |> insert 2 2 |> size) );
      ( "insert test SI 2" >:: fun _ ->
        assert_equal 1
          StrIntDictionary.(empty |> insert "a" 1 |> insert "a" 2 |> size) );
      ( "insert test II 3" >:: fun _ ->
        if TR.test then
          assert_equal "1 maps to 2; "
            IntIntDictionary.(empty |> insert 1 1 |> insert 1 2 |> to_string) );
      ( "insert test SI 3" >:: fun _ ->
        assert_equal
          [ ("a", 2) ]
          StrIntDictionary.(empty |> insert "a" 1 |> insert "a" 2 |> to_list) );
      ( "insert test II 4" >:: fun _ ->
        assert_equal
          [ (1, 1); (2, 2) ]
          IntIntDictionary.(empty |> insert 1 1 |> insert 2 2 |> to_list) );
      ( "insert test SI 4" >:: fun _ ->
        assert_equal
          [ ("a", 2); ("b", 1); ("c", 3) ]
          StrIntDictionary.(
            empty |> insert "b" 1 |> insert "a" 2 |> insert "c" 3 |> to_list) );
      ( "remove test II" >:: fun _ ->
        if TR.test then
          assert_equal 0
            IntIntDictionary.(empty |> insert 1 1 |> remove 1 |> size) );
      ( "remove test SI" >:: fun _ ->
        if TR.test then
          assert_equal 1
            StrIntDictionary.(empty |> insert "a" 1 |> remove "b" |> size) );
      ( "find test II" >:: fun _ ->
        assert_equal (Some 1) IntIntDictionary.(empty |> insert 1 1 |> find 1)
      );
      ( "find test SI" >:: fun _ ->
        assert_equal None StrIntDictionary.(empty |> insert "a" 1 |> find "b")
      );
      ( "member test II" >:: fun _ ->
        assert_equal true IntIntDictionary.(empty |> insert 1 1 |> member 1) );
      ( "member test SI 1" >:: fun _ ->
        assert_equal false
          StrIntDictionary.(empty |> insert "a" 1 |> member "b") );
      ( "member test SI 2" >:: fun _ ->
        if TR.test then
          assert_equal false
            StrIntDictionary.(empty |> insert "a" 1 |> remove "a" |> member "a")
      );
      ( "fold test II" >:: fun _ ->
        assert_equal 2
          IntIntDictionary.(
            empty |> insert 1 1 |> fold (fun k v init -> init + k + v) 0) );
      ( "fold test SI" >:: fun _ ->
        assert_equal "a1b2c3"
          StrIntDictionary.(
            empty |> insert "c" 3 |> insert "b" 2 |> insert "a" 1
            |> fold
                 (fun k v init -> init ^ k ^ StrIntDictionary.Value.to_string v)
                 "") );
      ( "to_list test IntSet" >:: fun _ ->
        assert_equal [] IntSet.(empty |> to_list) );
      ( "to_string test IntSet" >:: fun _ ->
        assert_equal "" IntSet.(empty |> to_string) );
      ( "is_empty test IntSet" >:: fun _ ->
        assert_equal true IntSet.(empty |> is_empty) );
      ("size test IntSet" >:: fun _ -> assert_equal 0 IntSet.(empty |> size));
      ( "insert test IntSet 1" >:: fun _ ->
        assert_equal 1 IntSet.(empty |> insert 1 |> size) );
      ( "insert test IntSet 2" >:: fun _ ->
        assert_equal false IntSet.(empty |> insert 1 |> is_empty) );
      ( "insert test IntSet 3" >:: fun _ ->
        assert_equal [ 2; 3; 4 ]
          IntSet.(empty |> insert 3 |> insert 2 |> insert 4 |> to_list) );
      ( "insert test IntSet 4" >:: fun _ ->
        assert_equal [ 2; 3 ]
          IntSet.(empty |> insert 3 |> insert 3 |> insert 2 |> to_list) );
      ( "member test IntSet 1" >:: fun _ ->
        assert_equal true IntSet.(empty |> insert 1 |> member 1) );
      ( "member test IntSet 2" >:: fun _ ->
        assert_equal false IntSet.(empty |> insert 1 |> member 2) );
      ( "remove test IntSet 1" >:: fun _ ->
        if TR.test then
          assert_equal false IntSet.(empty |> insert 1 |> remove 1 |> member 1)
      );
      ( "remove test IntSet 2" >:: fun _ ->
        if TR.test then
          assert_equal 1 IntSet.(empty |> insert 1 |> remove 2 |> size) );
      ( "fold test IntSet 1" >:: fun _ ->
        assert_equal 12
          IntSet.(
            empty |> insert 2 |> insert 1
            |> fold (fun x init -> (10 * init) + x) 0) );
      ( "fold test IntSet 2" >:: fun _ ->
        assert_equal 3
          IntSet.(
            empty |> insert 1 |> insert 2 |> fold (fun x init -> x + init) 0) );
      ( "union test IntSet 1" >:: fun _ ->
        assert_equal [ 1; 2; 3 ]
          IntSet.(
            empty |> insert 3 |> insert 1
            |> union (empty |> insert 2)
            |> to_list) );
      ( "union test IntSet 2" >:: fun _ ->
        assert_equal [ 3 ]
          IntSet.(empty |> insert 3 |> union (empty |> insert 3) |> to_list) );
      ( "intersect test IntSet 1" >:: fun _ ->
        assert_equal []
          IntSet.(empty |> insert 3 |> intersect (empty |> insert 2) |> to_list)
      );
      ( "intersect test IntSet 2" >:: fun _ ->
        if TR.test then
          assert_equal "3 "
            IntSet.(
              empty |> insert 3 |> insert 1
              |> intersect (empty |> insert 3)
              |> to_string) );
      ( "difference test IntSet 1" >:: fun _ ->
        assert_equal [ 1; 2 ]
          IntSet.(
            empty |> insert 3
            |> difference (empty |> insert 3 |> insert 1 |> insert 2)
            |> to_list) );
      ( "difference test IntSet 2" >:: fun _ ->
        assert_equal []
          IntSet.(
            empty |> insert 3 |> insert 1
            |> difference (empty |> insert 3)
            |> to_list) );
      ( "to_string test for TreeDictionary Exclusive 1" >:: fun _ ->
        if TR.test = false then
          assert_equal "3 maps to 3 which color is black; "
            IntIntDictionary.(empty |> insert 3 3 |> to_string) );
      ( "to_string test for TreeDictionary Exclusive 2" >:: fun _ ->
        if TR.test = false then
          assert_equal
            "1 maps to 1 which color is red; 3 maps to 3 which color is black; "
            IntIntDictionary.(empty |> insert 3 3 |> insert 1 1 |> to_string) );
    ]
end

module LDTests = Test (ListDictionary.Make) (TestList)
module TDTests = Test (TreeDictionary.Make) (NotTestList)

let ldtests = LDTests.tests
let tdtests = TDTests.tests

(* Scalability Test for TreeDictionary *)

module IITD = TreeDictionary.Make (Int) (Int)

let rec member_test n tree =
  if n = 0 then true else IITD.member n tree && member_test (n - 1) tree

let rec find_test n tree =
  if n = 0 then true else IITD.find n tree = Some n && find_test (n - 1) tree

let scaltest =
  if true then []
  else
    let rec build_tree n acc =
      if n = 0 then acc else build_tree (n - 1) (IITD.insert n n acc)
    in
    let large_tree = build_tree 2000000 IITD.empty in
    [
      ( "scal is_empty test" >:: fun _ ->
        assert_equal false (IITD.is_empty large_tree) );
      ( "scal size test" >:: fun _ ->
        assert_equal true (IITD.size large_tree = 2000000) );
      ( "scal member test 1" >:: fun _ ->
        assert_equal false (IITD.member ~-1 large_tree) );
      ( "scal member test 2" >:: fun _ ->
        assert_equal true (member_test 2000000 large_tree) );
      ( "scal find test" >:: fun _ ->
        assert_equal true (find_test 2000000 large_tree) );
    ]

(* Engine Tests *)

let alice_path = data_dir_prefix ^ "alice"

module EngineTest (TestEngine : Engine.E) = struct
  let alice_idx = TestEngine.index_of_dir alice_path
  let preamble_idx = TestEngine.index_of_dir preamble_path

  let tests =
    [
      ( "to_list test for alice" >:: fun _ ->
        assert_equal 3278 (List.length (alice_idx |> TestEngine.to_list)) );
      ( "to_list test for preamble" >:: fun _ ->
        assert_equal 38 (List.length (preamble_idx |> TestEngine.to_list)) );
      ( "words test for preamble 1" >:: fun _ ->
        assert_equal 38 (List.length (preamble_idx |> TestEngine.words)) );
      ( "words test for preamble 2" >:: fun _ ->
        assert_equal true
          (List.mem "america" (preamble_idx |> TestEngine.words)) );
      ( "and_not test for alice" >:: fun _ ->
        assert_equal [ "alice.txt" ] (TestEngine.and_not alice_idx [ "in" ] [])
      );
      ( "and_not test for preamble 1" >:: fun _ ->
        assert_equal [ "whole.txt" ]
          (TestEngine.and_not preamble_idx [ "we"; "the"; "people"; "of" ] [])
      );
      ( "and_not test for preamble 2" >:: fun _ ->
        assert_equal []
          (TestEngine.and_not preamble_idx
             [ "we"; "the"; "people"; "of" ]
             [ "america" ]) );
      ( "or_not test for preamble 1" >:: fun _ ->
        assert_equal
          [ "prefix.txt"; "whole.txt" ]
          (TestEngine.or_not preamble_idx [ "we" ] []) );
      ( "or_not test for preamble 2" >:: fun _ ->
        assert_equal [ "prefix.txt" ]
          (TestEngine.or_not preamble_idx
             [ "we"; "the"; "people"; "of" ]
             [ "america" ]) );
    ]
end

module ListEngineTest = EngineTest (ListEngine.E)
module TreeEngineTest = EngineTest (TreeEngine.E)

let listenginetest = ListEngineTest.tests
let treeenginetest = ListEngineTest.tests

let suite =
  "search test suite"
  >::: List.flatten
         [ ldtests; tdtests; scaltest; listenginetest; treeenginetest ]

let _ = run_test_tt_main suite
