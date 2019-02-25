# ppx_deriving_rlp

A preprocessor plugin that allows you to place `[@@deriving rlp]` annotations onto type definitions, to automatically generate `to_rlp_item` and `of_rlp_item` functions for that type.

To use it from a Dune project, put this preprocess item in your `dune` file:

```
(preprocess (pps ppx_deriving ppx_deriving_rlp))
```

Example:

```
type foo = A of int | B of float * bool
[@@deriving rlp]
```

```
# foo_to_rlp_item (A 5);;
- : Rlp.rlp_item =
Rlp.RlpItems
 [Rlp.RlpItem "A"; Rlp.RlpItem "\005"]
# foo_of_rlp_item (Rlp.RlpItems [Rlp.RlpItem "A"; Rlp.RlpItem "\005"]);;
- : foo = A 5
# foo_to_rlp_item (B (6.0, true));;
- : Rlp.rlp_item =
Rlp.RlpItems
 [Rlp.RlpItem "B";
  Rlp.RlpItems
   [Rlp.RlpItem "\x40\x18\x00\x00\x00\x00\x00\x00";
    Rlp.RlpItem "\001"]]
# foo_of_rlp_item (Rlp.RlpItems
                    [Rlp.RlpItem "B";
                     Rlp.RlpItems
                      [Rlp.RlpItem "\x40\x18\x00\x00\x00\x00\x00\x00";
                       Rlp.RlpItem "\001"]]);;
- : foo = B (6., true)
```
