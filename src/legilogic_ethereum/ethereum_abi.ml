(* ethereum-abi -- support for Ethereum contract ABI *)
(* see https://solidity.readthedocs.io/en/develop/abi-spec.html *)

open Legilogic_lib
open Lib
open Digesting
open Signing

(* TODO: somehow work with our static types UInt256, UInt64, etc.? Maybe using GADTs somehow? *)

type abi_type =
  (* uint<M>: unsigned integer type of M bits, 0 < M <= 256, M % 8 == 0. e.g. uint32, uint8, uint256 *)
  | Uint of int
  (* int<M>: two’s complement signed integer type of M bits, 0 < M <= 256, M % 8 == 0. *)
  | Int of int
  (* uint, int: synonyms for uint256, int256 respectively. For computing the function selector, uint256 and int256 have to be used. *)
  | UintDefault
  | IntDefault
  (* bool: equivalent to uint8 restricted to the values 0 and 1. For computing the function selector, bool is used. *)
  | Bool
  (* string: dynamic sized unicode string assumed to be UTF-8 encoded. *)
  | String
  (* fixed<M>x<N>: signed fixed-point decimal number of M bits, 8 <= M <= 256, M % 8 ==0, and 0 < N <= 80, which denotes the value v as v / (10 ** N). *)
  | Fixed of int * int
  (* ufixed<M>x<N>: unsigned variant of fixed<M>x<N>. *)
  | Ufixed of int * int
  (* fixed, ufixed: synonyms for fixed128x18, ufixed128x18 respectively. For computing the function selector, fixed128x18 and ufixed128x18 have to be used. *)
  | FixedDefault
  | UfixedDefault
  (* bytes<M>: binary type of M bytes, 0 < M <= 32. *)
  | Bytes of int
  (* bytes: dynamic sized byte sequence. *)
  | BytesDynamic
  (* <type>[M]: a fixed-length array of M elements, M >= 0, of the given type. *)
  | Array of int * abi_type
  (* <type>[]: a variable-length array of elements of the given type. *)
  | ArrayDynamic of abi_type
  (*  (T1,T2,...,Tn): tuple consisting of the types T1, …, Tn, n >= 0 *)
  | Tuple of abi_type list
  (*  function: an address (20 bytes) followed by a function selector (4 bytes). Encoded identical to bytes24. *)
  | Function
  (* address: equivalent to uint160, except for the assumed interpretation and language typing. For computing the function selector, address is used. *)
  | Address
[@@deriving show]

type abi_function_call = {function_name: string; parameters: (abi_value * abi_type) list}

(* values corresponding to the ABI types *)
and abi_value =
  (* bytes are big-endian *)
  | Uint_value of bytes
  (* bytes are big-endian *)
  | Int_value of bytes
  (* don't need values specifically for UintDefault and Intdefault *)
  | Bool_value of bool
  | String_value of string
  | Fixed_value of bytes
  | Ufixed_value of bytes
  (* for Bytes or BytesDynamic type *)
  | Bytes_value of bytes
  (* for Array or ArrayDynamic type *)
  | Array_value of abi_value list
  | Tuple_value of abi_value list
  | Function_value of Address.t * abi_function_call
  | Address_value of Address.t
[@@deriving show]

(* are constraints on types fulfilled *)
let rec is_valid_type = function
  | Uint m | Int m -> m mod 8 = 0 && 0 < m && m <= 256
  | Fixed (m, n) | Ufixed (m, n) -> 8 <= m && m <= 256 && m mod 8 = 0 && 0 < n && n <= 80
  | Bytes m -> 0 < m && m <= 32
  | Array (m, ty) -> m >= 0 && is_valid_type ty
  | ArrayDynamic ty -> is_valid_type ty
  | Tuple tys -> List.for_all is_valid_type tys
  (* no constraints *)
  | FixedDefault | UfixedDefault | BytesDynamic | String | Address | Function | Bool
  | UintDefault | IntDefault ->
    true

(* must get this right for hash of signature to work *)
let rec show_type_for_function_selector ty =
  if not (is_valid_type ty) then
    bork "Invalid Solidity type: %s" (show_abi_type ty) ;
  match ty with
  | Uint m -> "uint" ^ string_of_int m
  | Int m -> "int" ^ string_of_int m
  | UintDefault -> "uint256"
  | IntDefault -> "int256"
  | Fixed (m, n) -> "fixed" ^ string_of_int m ^ "x" ^ string_of_int n
  | Ufixed (m, n) -> "ufixed" ^ string_of_int m ^ "x" ^ string_of_int n
  | FixedDefault -> "fixed128x18"
  | UfixedDefault -> "ufixed128x18"
  | Bytes m -> "bytes" ^ string_of_int m
  | BytesDynamic -> "bytes"
  | Array (m, ty) -> show_type_for_function_selector ty ^ "[" ^ string_of_int m ^ "]"
  | ArrayDynamic ty -> show_type_for_function_selector ty ^ "[]"
  | Tuple tys ->
    let shown_tys = List.map show_type_for_function_selector tys in
    let tys_with_commas = String.concat "," shown_tys in
    "(" ^ tys_with_commas ^ ")"
  (* "show" prefixes the name with the module name, don't use it here *)
  | String -> "string"
  | Function -> "function"
  | Address -> "address"
  | Bool -> "bool"

(* build ABI values and types from OCaml values *)

let abi_string_of_string s = (String_value s, String)

(* intN and uintN builders *)
(* 2**n using Ocaml int *)
let rec pow2 n =
  if n < 0 then raise (Internal_error "pow2: expected nonnegative n") ;
  if n = 0 then 1 else 2 * pow2 (n - 1)

and get_big_endian_int_bytes bits n =
  (* create byte array representing big-endian representation *)
  let num_bytes = bits / 8 in
  let ff = 0xff in
  let get_byte ndx =
    (* if big-endian, preserve order, else reverse it *)
    let ndx1 = if Sys.big_endian then ndx else num_bytes - 1 - ndx in
    let bitshift = ndx1 * 8 in
    let shifted =
      (* check amount of shift; OCaml may return bogus result for too-large shift *)
      if bitshift < Sys.word_size then n lsr bitshift else 0
    in
    let c = shifted land ff in
    Char.chr c
  in
  Bytes.init num_bytes get_byte

and make_abi_intN_of_int bits n =
  if bits < 8 then bork "bits = %d must be at least 8" bits ;
  if bits mod 8 != 0 then
    bork "bits = %d is not a multiple of 8" bits ;
  let max_n = pow2 (bits - 1) - 1 in
  let min_n = -max_n - 1 in
  if n < min_n || n > max_n then
    bork "Value %d cannot be represented with type int%d" n bits ;
  let bytes = get_big_endian_int_bytes bits n in
  (Int_value bytes, Int bits)

and make_abi_uintN_of_int bits n =
  if bits < 8 then bork "bits = %d must be at least 8" bits ;
  if bits mod 8 != 0 then
    bork "bits = %d is not a multiple of 8" bits ;
  let max_n = pow2 bits - 1 in
  let min_n = 0 in
  if n < min_n || n > max_n then
    bork "Value %d cannot be represented with type uint%d" n bits ;
  let bytes = get_big_endian_int_bytes bits n in
  (Uint_value bytes, Uint bits)

let abi_int8_of_int = make_abi_intN_of_int 8

let abi_int16_of_int = make_abi_intN_of_int 16

let abi_int24_of_int = make_abi_intN_of_int 24

let abi_int32_of_int = make_abi_intN_of_int 32

let abi_int40_of_int = make_abi_intN_of_int 40

let abi_int48_of_int = make_abi_intN_of_int 48

let abi_int56_of_int = make_abi_intN_of_int 56

(* int is synonym for int256
   don't use make... because bounds checks will fail,
   and any int can be represented in 256 bits
*)
let abi_int_of_int n = (Int_value (get_big_endian_int_bytes 256 n), IntDefault)

let abi_uint8_of_int = make_abi_uintN_of_int 8

let abi_uint16_of_int = make_abi_uintN_of_int 16

let abi_uint24_of_int = make_abi_uintN_of_int 24

let abi_uint32_of_int = make_abi_uintN_of_int 32

let abi_uint40_of_int = make_abi_uintN_of_int 40

let abi_uint48_of_int = make_abi_uintN_of_int 48

let abi_uint56_of_int = make_abi_uintN_of_int 56

(* uint is synonym for uint256
   don't use make... because bounds checks will fail,
   and any int can be represented in 256 bits
*)
let abi_uint_of_int n = (Uint_value (get_big_endian_int_bytes 256 n), UintDefault)

let rec get_big_endian_int64_bytes n_64 =
  let ff_64 = Int64.of_int 0xff in
  let get_byte ndx =
    (* if big-endian, preserve order, else reverse it *)
    let ndx1 = if Sys.big_endian then ndx else 7 - ndx in
    let shifted = Int64.shift_right n_64 (ndx1 * 8) in
    let c_64 = Int64.logand shifted ff_64 in
    let c = Int64.to_int c_64 in
    Char.chr c
  in
  Bytes.init 8 get_byte

and abi_int64_of_int64 n_64 =
  let bytes = get_big_endian_int64_bytes n_64 in
  (Int_value bytes, Int 64)

(* convert positive int64 values to ABI uint64 values *)
let abi_uint64_of_int64 n_64 =
  if Int64.compare n_64 Int64.zero < 0 then
    raise (Internal_error "Can't represent negative int64 value as ABI Uint") ;
  match fst (abi_int64_of_int64 n_64) with
  | Int_value bytes -> (Uint_value bytes,Uint 64)
  | v -> raise (Internal_error (Printf.sprintf "Expected Int_value, got %s" (show_abi_value v)))

(* TODO: have builders for larger bit-width types that take an int64, or maybe Signed/Unsigned from
   integers library
*)

let abi_bytes_of_bytes bytes = (Bytes_value bytes, Bytes (Bytes.length bytes))

let abi_bytes_of_string s = abi_bytes_of_bytes (Bytes.of_string s)

let abi_bytes_dynamic_of_bytes bytes = (Bytes_value bytes, BytesDynamic)

let abi_bytes_dynamic_of_string s = abi_bytes_dynamic_of_bytes (Bytes.of_string s)

let abi_bool_of_bool b = (Bool_value b, Bool)

let abi_address_of_address address = (Address_value address, Address)

let abi_function_call_of_encoded_call address encoded_call =
  (Function_value (address, encoded_call), Function)

let abi_array_of_abi_values ty abi_typed_vals =
  if not (List.for_all (fun (_, ty') -> ty' = ty) abi_typed_vals) then
    raise (Internal_error "Array elements don't all match specified type") ;
  let len = List.length abi_typed_vals in
  let vals = List.map fst abi_typed_vals in
  (Array_value vals, Array (len, ty))

let abi_array_dynamic_of_abi_values ty abi_typed_vals =
  match fst (abi_array_of_abi_values ty abi_typed_vals) with
  | Array_value vals -> (vals, ArrayDynamic ty)
  | v -> raise (Internal_error (Printf.sprintf "Expected Array value, got %s" (show_abi_value v)))

let abi_tuple_of_abi_values abi_typed_vals =
  let vals = List.map fst abi_typed_vals in
  let tys = List.map snd abi_typed_vals in
  (Tuple_value vals, Tuple tys)

(* for Fixed encodings, num is an int or int64 which represents num * 10**n *)

let expected_int v =
  raise (Internal_error (Printf.sprintf "Expected Int value, got %s" (show_abi_value v)))

let abi_fixed8n_of_int n num =
  match fst (abi_int8_of_int num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (8, n))
  | v -> expected_int v

let abi_fixed16n_of_int n num =
  match fst (abi_int16_of_int num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (16, n))
  | v -> expected_int v

let abi_fixed24_of_int n num =
  match fst (abi_int24_of_int num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (24, n))
  | v -> expected_int v

let abi_fixed32_of_int n num =
  match fst (abi_int32_of_int num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (32, n))
  | v -> expected_int v

let abi_fixed40_of_int n num =
  match fst (abi_int40_of_int num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (40, n))
  | v -> expected_int v

let abi_fixed48_of_int n num =
  match fst (abi_int48_of_int num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (48, n))
  | v -> expected_int v

let abi_fixed56_of_int n num =
  match fst (abi_int56_of_int num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (56, n))
  | v -> expected_int v

let abi_fixed64_of_int64 n num =
  match fst (abi_int64_of_int64 num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (64, n))
  | v -> expected_int v

let abi_ufixed8n_of_int n num =
  match fst (abi_int8_of_int num) with
  | Int_value bytes -> (Ufixed (8, n), Ufixed_value bytes)
  | v -> expected_int v

let abi_ufixed16n_of_int n num =
  match fst (abi_int16_of_int num) with
  | Int_value bytes -> (Ufixed (16, n), Ufixed_value bytes)
  | v -> expected_int v

let abi_ufixed24_of_int n num =
  match fst (abi_int24_of_int num) with
  | Int_value bytes -> (Ufixed (24, n), Ufixed_value bytes)
  | v -> expected_int v

let abi_ufixed32_of_int n num =
  match fst (abi_int32_of_int num) with
  | Int_value bytes -> (Ufixed (32, n), Ufixed_value bytes)
  | v -> expected_int v

let abi_ufixed40_of_int n num =
  match fst (abi_int40_of_int num) with
  | Int_value bytes -> (Ufixed (40, n), Ufixed_value bytes)
  | v -> expected_int v

let abi_ufixed48_of_int n num =
  match fst (abi_int48_of_int num) with
  | Int_value bytes -> (Ufixed (48, n), Ufixed_value bytes)
  | v -> expected_int v

let abi_ufixed56_of_int n num =
  match fst (abi_int56_of_int num) with
  | Int_value bytes -> (Ufixed (56, n), Ufixed_value bytes)
  | v -> expected_int v

let abi_ufixed64_of_int64 n num =
  match fst (abi_int64_of_int64 num) with
  | Int_value bytes -> (Ufixed (64, n), Ufixed_value bytes)
  | v -> expected_int v

let function_signature function_call =
  (* parameter list is shown as a tuple over the passed types *)
  let tys = List.map snd function_call.parameters in
  let params = show_type_for_function_selector (Tuple tys) in
  function_call.function_name ^ params

let function_signature_hash = function_signature >> keccak256_string

(* first four bytes of call are the first four bytes of the Keccak256 hash of the
   function signature
*)
let encode_function_signature function_call =
  String.sub (function_signature_hash function_call) 0 4

(* encoding of parameter depends on classification of types as static or dynamic *)
let rec is_dynamic_type = function
  | BytesDynamic | String | ArrayDynamic _ -> true
  | Array (_, elt_ty) when is_dynamic_type elt_ty -> true
  | Tuple tys when List.exists is_dynamic_type tys -> true
  (* static type *)
  | _ -> false

(* for a given type, the number of bytes it needs in the "heads" part of the encoding *)
let rec get_head_space ty =
  match ty with
  (* static types with 32-byte representation *)
  | Uint _ | Int _ | UintDefault | IntDefault | Bool | Bytes _ | Fixed _ | Ufixed _
  |FixedDefault | UfixedDefault | Function | Address ->
    32
  (* dynamic types where we encode a uint256 offset *)
  | BytesDynamic | String | ArrayDynamic _ -> 32
  (* cases for aggregates *)
  | Array (m, ty') ->
    if is_dynamic_type ty then 32
    else (* sum of length encoding and length of element encodings *)
      32 + (m * get_head_space ty')
  | Tuple tys ->
    if is_dynamic_type ty then 32 else List.fold_right ( + ) (List.map get_head_space tys) 0

(* encoding is driven by types; check that the value matches the type *)
let rec encode_abi_value v ty =
  match (v, ty) with
  | Uint_value bytes, Uint m ->
    let bytes_len = Bytes.length bytes in
    if m / 8 != bytes_len then
      raise
        (Internal_error (Printf.sprintf "have type uint%d, got value with %d bytes" m bytes_len)) ;
    (* left-pad to 32 bytes *)
    let padding = Bytes.make (32 - bytes_len) '\000' in
    Bytes.cat padding bytes
  | Int_value bytes, Int m ->
    let bytes_len = Bytes.length bytes in
    if m / 8 != bytes_len then
      raise
        (Internal_error (Printf.sprintf "have type int%d, got value with %d bytes" m bytes_len)) ;
    (* left-pad to 32 bytes *)
    let is_negative = Char.code (Bytes.get bytes 0) land 0b10000000 = 0 in
    let pad_byte = if is_negative then Char.chr 0xff else '\000' in
    let padding = Bytes.make (32 - bytes_len) pad_byte in
    Bytes.cat padding bytes
  | Uint_value _bytes, UintDefault -> encode_abi_value v (Uint 256)
  | Int_value _bytes, IntDefault -> encode_abi_value v (Int 256)
  | String_value s, String ->
    (* assume that s is already UTF-8, so just convert it to bytes *)
    let bytes = Bytes.of_string s in
    encode_abi_value (Bytes_value bytes) BytesDynamic
  | Bool_value b, Bool ->
    let num = if b then 1 else 0 in
    let uint8_val, uint8_ty = abi_uint8_of_int num in
    encode_abi_value uint8_val uint8_ty
  | Ufixed_value bys, Ufixed (m, _n) when Bytes.length bys = m / 8 ->
    encode_abi_value (Uint_value bys) (Uint 256)
  | Ufixed_value _bys, UfixedDefault -> encode_abi_value v (Ufixed (128, 18))
  | Fixed_value bys, Fixed (m, _n) when Bytes.length bys = m / 8 ->
    encode_abi_value (Int_value bys) (Int 256)
  | Fixed_value _bys, FixedDefault -> encode_abi_value v (Fixed (128, 18))
  | Bytes_value bys, Bytes n when Bytes.length bys = n ->
    let padding = Bytes.make (32 - Bytes.length bys) '\000' in
    (* right-pad to 32 bytes *)
    Bytes.cat bys padding
  | Bytes_value bys, BytesDynamic ->
    (* uint256 encoding of length of bys, then bys, then padding to make multiple of 32 bytes *)
    let len = Bytes.length bys in
    (* use 64 bits to make uniform on all architectures *)
    let len_64 = Int64.of_int len in
    let lo_8_bytes = get_big_endian_int64_bytes len_64 in
    let len_padding = Bytes.make 24 '\000' in
    let bytes_32 = Bytes.cat len_padding lo_8_bytes in
    let len_encoding = encode_abi_value (Uint_value bytes_32) (Uint 256) in
    let total_len = Bytes.length len_encoding + len in
    let extra_padding_len =
      let mod32 = total_len mod 32 in
      if mod32 = 0 then 0 else 32 - mod32
    in
    let extra_padding = Bytes.make extra_padding_len '\000' in
    Bytes.concat Bytes.empty [len_encoding; bys; extra_padding]
  | Fixed_value _, Fixed (_m, _n) -> bottom () (* TODO *)
  | Ufixed_value _, Ufixed (_m, _n) -> bottom () (* TODO *)
  | Address_value address, Address ->
    let bytes = Ethereum_util.bytes_of_address address in
    encode_abi_value (Uint_value bytes) (Uint 160)
  | Function_value (address, function_call), Function ->
    let address_bits = Address.to_big_endian_bits address in
    let signature_bits = encode_function_signature function_call in
    let bytes = address_bits ^ signature_bits |> Bytes.of_string in
    encode_abi_value (Bytes_value bytes) (Bytes 24)
  | Array_value elts, Array (_m, ty) ->
    let element_encodings = List.map (fun elt -> encode_abi_value elt ty) elts in
    Bytes.concat Bytes.empty element_encodings
  | Array_value elts, ArrayDynamic ty ->
    let len = List.length elts in
    let len_encoding = get_big_endian_int_bytes 256 len in
    let element_encodings = List.map (fun elt -> encode_abi_value elt ty) elts in
    Bytes.cat len_encoding (Bytes.concat Bytes.empty element_encodings)
  | Tuple_value vals, Tuple tys ->
    let val_tys = List.map2 (fun v ty -> (v, ty)) vals tys in
    let arrays_len = List.length val_tys in
    let heads = Array.make arrays_len Bytes.empty in
    let tails = Array.make arrays_len Bytes.empty in
    let cum_tail_lens = Array.make arrays_len 0 in
    let head_spaces = List.map get_head_space tys in
    let total_head_space = List.fold_right ( + ) head_spaces 0 in
    let _ =
      List.iteri
        (fun ndx (v, ty) ->
           let _ =
             let encoding = encode_abi_value v ty in
             (* update heads entry *)
             if is_dynamic_type ty then (
               let offset =
                 if ndx = 0 then total_head_space else total_head_space + cum_tail_lens.(ndx - 1)
               in
               let offset_bytes = get_big_endian_int_bytes 256 offset in
               heads.(ndx) <- offset_bytes ; tails.(ndx) <- encoding )
             else (* tails already initialized with empty bytes *)
               heads.(ndx) <- encoding
           in
           let curr_len = Bytes.length tails.(ndx) in
           cum_tail_lens.(ndx)
           <- (if ndx = 0 then curr_len else cum_tail_lens.(ndx - 1) + curr_len) )
        val_tys
    in
    let heads_bytes = Array.fold_right Bytes.cat heads Bytes.empty in
    let tails_bytes = Array.fold_right Bytes.cat tails Bytes.empty in
    Bytes.cat heads_bytes tails_bytes
  | _ ->
    raise
      (Internal_error
         (Printf.sprintf "Value to be encoded: %s\nDoes not match its type: %s"
            (show_abi_value v) (show_abi_type ty)))

(* an encoding of the function call is what we pass to Ethereum in a transaction *)
let encode_function_call function_call =
  let encoded_signature = encode_function_signature function_call |> Bytes.of_string in
  let param_val, param_ty = abi_tuple_of_abi_values function_call.parameters in
  let encoded_params = encode_abi_value param_val param_ty in
  Bytes.cat encoded_signature encoded_params

module Test = struct
  open Ethereum_util.Test

  let%test "int64-of-int64-encoding" =
    let ff = 0xff in
    let ff_64 = Int64.of_int ff in
    match abi_int64_of_int64 ff_64 with
    | Int_value bytes, Int 64 ->
      (* ff_64 has an all-1s lo byte on little-endian Intel CPU
         in big-endian representation, should have all-1s hi byte
         all other bytes should be 0
      *)
      let hi_byte = Char.code (Bytes.get bytes 7) in
      let other_bytes_64 =
        let rec get_byte ndx accum =
          if ndx < 0 then accum else get_byte (ndx - 1) (Bytes.get bytes ndx :: accum)
        in
        get_byte 6 []
      in
      Bytes.length bytes = 8 && hi_byte = ff && List.for_all (Char.equal '\000') other_bytes_64
    | _ -> false

  (* similar test, but provide int that fits in 56 bits instead of int64 *)
  let%test "int64-of-int-encoding" =
    let ff = 0xff in
    match abi_int56_of_int ff with
    | Int_value bytes, Int 56 ->
      (* ff has an all-1s lo byte on little-endian Intel CPU
         in big-endian representation, should have all-1s hi byte
         all other bytes should be 0
      *)
      let hi_byte = Char.code (Bytes.get bytes 6) in
      let other_bytes =
        let rec get_byte ndx accum =
          if ndx < 0 then accum else get_byte (ndx - 1) (Bytes.get bytes ndx :: accum)
        in
        get_byte 5 []
      in
      Bytes.length bytes = 7 && hi_byte = ff && List.for_all (Char.equal '\000') other_bytes
    | _ -> false

  let%test "contract-call-encoding-1" =
    (* first example from ABI spec *)
    let param1 = abi_uint32_of_int 69 in
    let param2 = abi_bool_of_bool true in
    let call = {function_name= "baz"; parameters= [param1; param2]} in
    expect_0x_bytes "encode_function_call result"
      "0xcdcd77c000000000000000000000000000000000000000000000000000000000000000450000000000000000000000000000000000000000000000000000000000000001"
      (encode_function_call call);
    true

  let%test "contract-call-encoding-2" =
    (* second example from ABI spec *)
    let bytes1, ty1 = abi_bytes_of_string "abc" in
    let bytes2, ty2 = abi_bytes_of_string "def" in
    assert (ty1 = ty2 && ty1 = Bytes 3) ;
    let array_val = Array_value [bytes1; bytes2] in
    let array_ty = Array (2, ty1) in
    let param = (array_val, array_ty) in
    let call = {function_name= "bar"; parameters= [param]} in
    expect_0x_bytes "encode_function_call result"
      "0xfce353f661626300000000000000000000000000000000000000000000000000000000006465660000000000000000000000000000000000000000000000000000000000"
      (encode_function_call call);
    true

  let%test "contract-call-encoding-3" =
    (* third example from ABI spec *)
    let param1 = abi_bytes_dynamic_of_string "dave" in
    let param2 = abi_bool_of_bool true in
    let param3_array = List.map (fun n -> fst (abi_uint_of_int n)) [1; 2; 3] in
    let param3_val = Array_value param3_array in
    (* For computing the function selector, uint256 and int256 have to be used. *)
    let param3 = (param3_val, ArrayDynamic (Uint 256)) in
    let call = {function_name= "sam"; parameters= [param1; param2; param3]} in
    expect_0x_bytes "encode_function_call result"
      "0xa5643bf20000000000000000000000000000000000000000000000000000000000000060000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000464617665000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000003"
      (encode_function_call call);
    true

  let%test "contract-call-encoding-4" =
    (* fourth example from ABI spec *)
    let param1 = abi_uint_of_int 0x123 in
    let param2 =
      let num1, ty1 = abi_uint32_of_int 0x456 in
      let num2, ty2 = abi_uint32_of_int 0x789 in
      assert (ty1 = ty2 && ty1 = Uint 32) ;
      (Array_value [num1; num2], ArrayDynamic ty1)
    in
    let param3 = abi_bytes_of_string "1234567890" in
    let param4 = abi_bytes_dynamic_of_string "Hello, world!" in
    let call = {function_name= "f"; parameters= [param1; param2; param3; param4]} in
    expect_0x_bytes "encode_function_call result"
      "0x8be6524600000000000000000000000000000000000000000000000000000000000001230000000000000000000000000000000000000000000000000000000000000080313233343536373839300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e0000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000004560000000000000000000000000000000000000000000000000000000000000789000000000000000000000000000000000000000000000000000000000000000d48656c6c6f2c20776f726c642100000000000000000000000000000000000000"
      (encode_function_call call);
    true

  let%test "contract-parameters-encoding" =
    (* from our Solidity hello.sol example *)
    let parameters, ty = abi_tuple_of_abi_values [(String_value "Hello, world!", String)] in
    expect_0x_bytes "encode_abi_value"
      "0x0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000d48656c6c6f2c20776f726c642100000000000000000000000000000000000000"
      (encode_abi_value parameters ty);
    true
end
