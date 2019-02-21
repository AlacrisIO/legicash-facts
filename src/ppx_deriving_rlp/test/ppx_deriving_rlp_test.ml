open Ppx_deriving_rlp_runtime
open Ppx_deriving_rlp_test_data
open Ppx_deriving_rlp_alias_adt
open OUnit2

let show_string s = "\"" ^ String.escaped s ^ "\""
let buffer_to_string b = Bytes.to_string (Buffer.to_bytes b)

let check_convert_rlp to_rlp of_rlp marshal_rlp unmarshal_rlp ~ctxt:ctxt data rlp str =
  let buffer = Buffer.create 32
  and pre = "\x902re3mi1do1do5sol"
  and post = "\x915sol7ti2re5sol1do" in
  let pre_offset = String.length pre in
  let post_offset = pre_offset + String.length str in
  (* assert_equal expected actual *)
  assert_equal ~ctxt ~printer:Rlp.show_rlp_item rlp (to_rlp data);
  assert_equal ~ctxt data (of_rlp rlp);
  assert_equal ~ctxt ~printer:show_string str (Rlp_encode.rlp_item_to_rlp rlp);
  assert_equal ~ctxt ~printer:Rlp.show_rlp_item rlp (Rlp_decode.rlp_item_of_rlp str);
  Buffer.add_string buffer pre;
  marshal_rlp buffer data;
  Buffer.add_string buffer post;
  assert_equal ~ctxt ~printer:show_string (pre ^ str ^ post) (buffer_to_string buffer);
  assert_equal ~ctxt (data, post_offset) (unmarshal_rlp pre_offset (buffer_to_string buffer))

let check_z_rlp = check_convert_rlp [%rlp: Z.t].to_rlp_item [%rlp: Z.t].of_rlp_item [%rlp: Z.t].marshal_rlp [%rlp: Z.t].unmarshal_rlp
let check_float_rlp = check_convert_rlp [%rlp: float].to_rlp_item [%rlp: float].of_rlp_item [%rlp: float].marshal_rlp [%rlp: float].unmarshal_rlp
let check_alias_int_rlp = check_convert_rlp alias_int_to_rlp_item alias_int_of_rlp_item alias_int_marshal_rlp alias_int_unmarshal_rlp
let check_alias_list_rlp = check_convert_rlp alias_list_to_rlp_item alias_list_of_rlp_item alias_list_marshal_rlp alias_list_unmarshal_rlp
let check_alias_unit_rlp = check_convert_rlp alias_unit_to_rlp_item alias_unit_of_rlp_item alias_unit_marshal_rlp alias_unit_unmarshal_rlp
let check_foo_rlp = check_convert_rlp foo_to_rlp_item foo_of_rlp_item foo_marshal_rlp foo_unmarshal_rlp
let check_loi_rlp = check_convert_rlp loi_to_rlp_item loi_of_rlp_item loi_marshal_rlp loi_unmarshal_rlp
let check_wrapped_list1_rlp = check_convert_rlp wrapped_list1_to_rlp_item wrapped_list1_of_rlp_item wrapped_list1_marshal_rlp wrapped_list1_unmarshal_rlp
let check_wrapped_list2_rlp = check_convert_rlp wrapped_list2_to_rlp_item wrapped_list2_of_rlp_item wrapped_list2_marshal_rlp wrapped_list2_unmarshal_rlp
let check_wrapped_list3_rlp = check_convert_rlp wrapped_list3_to_rlp_item wrapped_list3_of_rlp_item wrapped_list3_marshal_rlp wrapped_list3_unmarshal_rlp
let check_seq_tree_map_str_str_rlp = check_convert_rlp [%rlp: (string, string) seq_tree_map].to_rlp_item
                                                       [%rlp: (string, string) seq_tree_map].of_rlp_item
                                                       (seq_tree_map_marshal_rlp string_to_rlp_item string_to_rlp_item)
                                                       (seq_tree_map_unmarshal_rlp string_of_rlp_item string_of_rlp_item)

let check_t_alias_rlp = check_convert_rlp [%rlp: t_alias].to_rlp_item [%rlp: t_alias].of_rlp_item [%rlp: t_alias].marshal_rlp [%rlp: t_alias].unmarshal_rlp

let test_z1 ctxt = check_z_rlp ~ctxt (Z.of_int 5) (RlpItem "\005") "\005"
let test_z2 ctxt = check_z_rlp ~ctxt (Z.of_int (-10)) (RlpItems [RlpItem "\010"]) "\xc1\010"
let test_z3 ctxt = check_z_rlp ~ctxt (Z.of_string "98765432109876543210")
                                     (RlpItem "\x05\x5A\xA5\x4D\x38\xE5\x26\x7E\xEA")
                                     "\x89\x05\x5A\xA5\x4D\x38\xE5\x26\x7E\xEA"

let test_int1 ctxt = check_alias_int_rlp ~ctxt 5 (RlpItem "\005") "\005"
let test_int2 ctxt = check_alias_int_rlp ~ctxt (-10) (RlpItems [RlpItem "\010"]) "\xc1\010"
let test_list2 ctxt = check_alias_list_rlp ~ctxt [6] (RlpItems [RlpItem "\006"]) "\xc1\006"
let test_unit2 ctxt = check_alias_unit_rlp ~ctxt () (RlpItems []) "\xc0"

let test_float1 ctxt = assert_equal ~ctxt ~printer:Rlp.show_rlp_item
                                    (RlpItem "\x3f\xf2\x49\x24\x92\x49\x24\x92")
                                    (float_to_rlp_item 1.1428571428571428)
let test_float2 ctxt = assert_equal ~ctxt ~printer:string_of_float
                                    1.1428571428571428
                                    (float_of_rlp_item (RlpItem "\x3f\xf2\x49\x24\x92\x49\x24\x92"))
let test_float3 ctxt = check_float_rlp ~ctxt
                                       1.1428571428571428
                                       (RlpItem "\x3f\xf2\x49\x24\x92\x49\x24\x92")
                                       "\x88\x3f\xf2\x49\x24\x92\x49\x24\x92"

let test3 ctxt = check_foo_rlp ~ctxt (A 5) (RlpItems [RlpItem ""; RlpItem "\005"]) "\xc2\x80\005"
let test4 ctxt = check_foo_rlp ~ctxt
                               (B (6.0, true))
                               (RlpItems [RlpItem "\001"; RlpItems [RlpItem "\x40\x18\x00\x00\x00\x00\x00\x00"; RlpItem "\001"]])
                               "\xcc\001\xca\x88\x40\x18\x00\x00\x00\x00\x00\x00\001"



let test5 ctxt = check_loi_rlp ~ctxt Loimt (RlpItem "") "\x80"
let test6 ctxt = check_loi_rlp ~ctxt
                               (Loicons { first = 1; rest = Loimt })
                               (RlpItems [RlpItem "\001"; RlpItems [RlpItem "\001"; RlpItem ""]])
                               "\xc4\001\xc2\001\x80"
let test7 ctxt = check_loi_rlp ~ctxt 
                               (Loicons { first = 3; rest = Loicons { first = 5; rest = Loicons { first = 8; rest = Loimt } } })
                               (RlpItems [RlpItem "\001"; RlpItems [RlpItem "\003";
                                  RlpItems [RlpItem "\001"; RlpItems [RlpItem "\005"; 
                                    RlpItems [RlpItem "\001"; RlpItems [RlpItem "\008"; RlpItem ""]]]]]])
                               "\xcc\001\xca\003\xc8\001\xc6\005\xc4\001\xc2\008\x80"


let test8 ctxt = check_wrapped_list1_rlp ~ctxt (Wrap1 [1]) (RlpItems [RlpItem ""; RlpItems [RlpItem "\001"]]) "\xc3\x80\xc1\001"
let test9 ctxt = check_wrapped_list2_rlp ~ctxt (Wrap2 { value = [1] }) (RlpItems [RlpItem ""; RlpItems [RlpItem "\001"]]) "\xc3\x80\xc1\001"
let test10 ctxt = check_wrapped_list3_rlp ~ctxt ({ value = [1] }) (RlpItems [RlpItem "\001"]) "\xc1\001"

let test_stmss1 ctxt = check_seq_tree_map_str_str_rlp ~ctxt
                         (StmLeaf "nemo")
                         (RlpItems [RlpItem ""; RlpItem "nemo"])
                         "\xc6\x80\x84nemo"
let test_stmss2 ctxt = check_seq_tree_map_str_str_rlp ~ctxt 
                         (StmNode [("A", StmLeaf "apple");
                                   ("B", StmNode [("A", StmNode [("N", StmLeaf "banana");
                                                                 ("T", StmLeaf "batter")]);
                                                  ("U", StmLeaf "bubbly")]);
                                   ("C", StmLeaf "cheese")])
                         (RlpItems 
                          [RlpItem "\001";
                           RlpItems 
                           [RlpItems [RlpItem "A"; RlpItems [RlpItem ""; RlpItem "apple"]];
                            RlpItems [RlpItem "B"; 
                                      RlpItems
                                      [RlpItem "\001";
                                       RlpItems
                                       [RlpItems [RlpItem "A"; 
                                                  RlpItems 
                                                  [RlpItem "\001";
                                                   RlpItems
                                                   [RlpItems [RlpItem "N"; RlpItems [RlpItem ""; RlpItem "banana"]];
                                                    RlpItems [RlpItem "T"; RlpItems [RlpItem ""; RlpItem "batter"]]]]];
                                        RlpItems [RlpItem "U"; RlpItems [RlpItem ""; RlpItem "bubbly"]]]]];
                            RlpItems [RlpItem "C"; RlpItems [RlpItem ""; RlpItem "cheese"]]]])
                         "\248C\001\248@\201A\199\128\133apple\234B\232\001\230\218A\216\001\214\202N\200\128\134banana\202T\200\128\134batter\202U\200\128\134bubbly\202C\200\128\134cheese"

let test_t_alias1 ctxt = check_t_alias_rlp ~ctxt
                           (O [("name", O [("A", S "Alice");
                                           ("B", S "Bob");
                                           ("C", S "Charlie");
                                           ("E", S "Eve")]);
                               ("send", L [L [S "A"; S "B"; N 2187];
                                           L [S "B"; S "A"; N 24601];
                                           L [S "C"; S "B"; N 713];
                                           L [S "B"; S "C"; N 24601]]);
                               ("see", O [("A", L [L [S "B"; S "A"; N 24601]]);
                                          ("B", L [L [S "A"; S "B"; N 2187];
                                                   L [S "C"; S "B"; N 713]]);
                                          ("C", L [L [S "B"; S "C"; N 24601]]);
                                          ("E", L [L [S "A"; S "B"; N 2187];
                                                L [S "B"; S "A"; N 24601];
                                                L [S "C"; S "B"; N 713];
                                                L [S "B"; S "C"; N 24601]])])])
                           (Rlp.RlpItems
                            [(Rlp.RlpItem "\003");
                             (Rlp.RlpItems
                              [(Rlp.RlpItems
                                [(Rlp.RlpItem "name");
                                 (Rlp.RlpItems
                                  [(Rlp.RlpItem "\003");
                                   (Rlp.RlpItems
                                    [(Rlp.RlpItems [(Rlp.RlpItem "A"); (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "Alice")])]);
                                     (Rlp.RlpItems [(Rlp.RlpItem "B"); (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "Bob")])]);
                                     (Rlp.RlpItems [(Rlp.RlpItem "C"); (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "Charlie")])]);
                                     (Rlp.RlpItems [(Rlp.RlpItem "E"); (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "Eve")])])])])]);
                               (Rlp.RlpItems
                                [(Rlp.RlpItem "send");
                                 (Rlp.RlpItems
                                  [(Rlp.RlpItem "\002");
                                   (Rlp.RlpItems
                                    [(Rlp.RlpItems
                                      [(Rlp.RlpItem "\002");
                                       (Rlp.RlpItems
                                        [(Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "A")]);
                                         (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "B")]);
                                         (Rlp.RlpItems [(Rlp.RlpItem ""); (Rlp.RlpItem "\008\139")])])]);
                                      (Rlp.RlpItems
                                       [(Rlp.RlpItem "\002");
                                        (Rlp.RlpItems
                                         [(Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "B")]);
                                          (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "A")]);
                                          (Rlp.RlpItems [(Rlp.RlpItem ""); (Rlp.RlpItem "\096\025")])])]);
                                      (Rlp.RlpItems
                                       [(Rlp.RlpItem "\002");
                                        (Rlp.RlpItems
                                         [(Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "C")]);
                                          (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "B")]);
                                          (Rlp.RlpItems [(Rlp.RlpItem ""); (Rlp.RlpItem "\002\201")])])]);
                                      (Rlp.RlpItems
                                       [(Rlp.RlpItem "\002");
                                        (Rlp.RlpItems
                                         [(Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "B")]);
                                          (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "C")]);
                                          (Rlp.RlpItems [(Rlp.RlpItem ""); (Rlp.RlpItem "\096\025")])])])])])]);
                               (Rlp.RlpItems
                                [(Rlp.RlpItem "see");
                                 (Rlp.RlpItems
                                  [(Rlp.RlpItem "\003");
                                   (Rlp.RlpItems
                                    [(Rlp.RlpItems
                                      [(Rlp.RlpItem "A");
                                       (Rlp.RlpItems
                                        [(Rlp.RlpItem "\002");
                                         (Rlp.RlpItems
                                          [(Rlp.RlpItems
                                            [(Rlp.RlpItem "\002");
                                             (Rlp.RlpItems
                                              [(Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "B")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "A")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem ""); (Rlp.RlpItem "\096\025")])])])])])]);
                                     (Rlp.RlpItems
                                      [(Rlp.RlpItem "B");
                                       (Rlp.RlpItems
                                        [(Rlp.RlpItem "\002");
                                         (Rlp.RlpItems
                                          [(Rlp.RlpItems
                                            [(Rlp.RlpItem "\002");
                                             (Rlp.RlpItems
                                              [(Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "A")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "B")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem ""); (Rlp.RlpItem "\008\139")])])]);
                                           (Rlp.RlpItems
                                            [(Rlp.RlpItem "\002");
                                             (Rlp.RlpItems
                                              [(Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "C")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "B")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem ""); (Rlp.RlpItem "\002\201")])])])])])]);
                                     (Rlp.RlpItems
                                      [(Rlp.RlpItem "C");
                                       (Rlp.RlpItems
                                        [(Rlp.RlpItem "\002");
                                         (Rlp.RlpItems
                                          [(Rlp.RlpItems
                                            [(Rlp.RlpItem "\002");
                                             (Rlp.RlpItems
                                              [(Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "B")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "C")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem ""); (Rlp.RlpItem "\096\025")])])])])])]);
                                     (Rlp.RlpItems
                                      [(Rlp.RlpItem "E");
                                       (Rlp.RlpItems
                                        [(Rlp.RlpItem "\002");
                                         (Rlp.RlpItems
                                          [(Rlp.RlpItems
                                            [(Rlp.RlpItem "\002");
                                             (Rlp.RlpItems
                                              [(Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "A")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "B")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem ""); (Rlp.RlpItem "\008\139")])])]);
                                           (Rlp.RlpItems
                                            [(Rlp.RlpItem "\002");
                                             (Rlp.RlpItems
                                              [(Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "B")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "A")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem ""); (Rlp.RlpItem "\096\025")])])]);
                                           (Rlp.RlpItems
                                            [(Rlp.RlpItem "\002");
                                             (Rlp.RlpItems
                                              [(Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "C")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "B")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem ""); (Rlp.RlpItem "\002\201")])])]);
                                           (Rlp.RlpItems
                                            [(Rlp.RlpItem "\002");
                                             (Rlp.RlpItems
                                              [(Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "B")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem "\001"); (Rlp.RlpItem "C")]);
                                               (Rlp.RlpItems [(Rlp.RlpItem ""); (Rlp.RlpItem "\096\025")])])])])])])])])])])])
                           "\249\001\t\003\249\001\005\238\132name\232\003\230\201A\199\001\133Alice\199B\197\001\131Bob\203C\201\001\135Charlie\199E\197\001\131Eve\248B\132send\248;\002\2488\205\002\203\194\001A\194\001B\196\128\130\008\139\205\002\203\194\001B\194\001A\196\128\130\096\025\205\002\203\194\001C\194\001B\196\128\130\002\201\205\002\203\194\001B\194\001C\196\128\130\096\025\248\144\131see\248\138\003\248\135\210A\208\002\206\205\002\203\194\001B\194\001A\196\128\130\096\025\224B\222\002\220\205\002\203\194\001A\194\001B\196\128\130\008\139\205\002\203\194\001C\194\001B\196\128\130\002\201\210C\208\002\206\205\002\203\194\001B\194\001C\196\128\130\096\025\248>E\248;\002\2488\205\002\203\194\001A\194\001B\196\128\130\008\139\205\002\203\194\001B\194\001A\196\128\130\096\025\205\002\203\194\001C\194\001B\196\128\130\002\201\205\002\203\194\001B\194\001C\196\128\130\096\025"

let suite =
"ppx_deriving_rlp_test">:::
 ["test_z1">:: test_z1;
  "test_z2">:: test_z2;
  "test_z3">:: test_z3;
  "test_int1">:: test_int1;
  "test_int2">:: test_int2;
  "test_list2">:: test_list2;
  "test_unit2">:: test_unit2;
  "test_float1">:: test_float1;
  "test_float2">:: test_float2;
  "test_float3">:: test_float3;
  "test3">:: test3;
  "test4">:: test4;
  "test5">:: test5;
  "test6">:: test6;
  "test7">:: test7;
  "test8">:: test8;
  "test9">:: test9;
  "test10">:: test10;
  "test_stmss1">:: test_stmss1;
  "test_stmss2">:: test_stmss2;
  "test_t_alias1">:: test_t_alias1]

let () = run_test_tt_main suite
