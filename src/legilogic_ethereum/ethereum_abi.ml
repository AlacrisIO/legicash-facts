(* ethereum-abi -- support for Ethereum contract ABI *)
(* see https://solidity.readthedocs.io/en/develop/abi-spec.html *)

open Legilogic_lib
open Lib
open Integer
open Digesting
open Types
open Signing
open Yojsoning

open Ethereum_chain

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


let equal (eval1 : abi_value) (eval2 : abi_value) : bool =
  match eval1 with
  | Uint_value x1 ->
     (match eval2 with
      | Uint_value x2 -> (Bytes.equal x1 x2)
      | _ -> false)
  | Int_value x1 ->
     (match eval2 with
      | Int_value x2 -> (Bytes.equal x1 x2)
      | _ -> false)
  | Bool_value x1 ->
     (match eval2 with
      | Bool_value x2 -> x1 == x2
      | _ -> false)
  | Address_value x1 ->
     (match eval2 with
      | Address_value x2 -> let str1 = Address.to_string x1 in
                            let str2 = Address.to_string x2 in
                            (String.equal str1 str2)
      | _ -> false)
  | Bytes_value x1 ->
     (match eval2 with
      | Bytes_value x2 -> (Bytes.equal x1 x2)
      | _ -> false)
  | _ -> bork "Missing code"
                   
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

let abi_string s = (String_value s, String)

(* intN and uintN builders *)
(* 2**n using Ocaml int *)
let rec pow2 n =
  if n < 0 then bork "pow2: expected nonnegative n" ;
  if n = 0 then 1 else 2 * pow2 (n - 1)

let big_endian_bytes_of_nat xint_name num_bits z nat =
  if num_bits < 8 then
    bork "num_bits = %d must be at least 8" num_bits ;
  if num_bits mod 8 != 0 then
    bork "num_bits = %d is not a multiple of 8" num_bits ;
  if num_bits > 256 then
    bork "num_bits = %d is too large" num_bits ;
  if Z.sign nat < 0 || Z.numbits nat > num_bits then
    bork "Value %s cannot be represented with type %s%d" (Z.to_string z) xint_name num_bits ;
  Bytes.of_string (big_endian_bits_of_nat num_bits nat)

let uint_of_big_endian_bytes num_bits bytes =
  if num_bits < 8 then
    bork "num_bits = %d must be at least 8" num_bits ;
  if num_bits mod 8 != 0 then
    bork "num_bits = %d is not a multiple of 8" num_bits ;
  if num_bits > 256 then
    bork "num_bits = %d is too large" num_bits ;
  nat_of_big_endian_bits num_bits (Bytes.to_string bytes)

let abi_value_to_uintN n evalue =
  match evalue with
  | Uint_value x -> uint_of_big_endian_bytes n x
  | _ -> bork "Wrong input type"
  
let abi_value_to_uint8 = abi_value_to_uintN 8

let abi_value_to_uint16 = abi_value_to_uintN 16

let abi_value_to_uint24 = abi_value_to_uintN 24

let abi_value_to_uint32 = abi_value_to_uintN 32

let abi_value_to_uint40 = abi_value_to_uintN 40

let abi_value_to_uint48 = abi_value_to_uintN 48

let abi_value_to_uint56 = abi_value_to_uintN 56

let abi_value_to_uint64 = abi_value_to_uintN 64

let abi_value_to_uint = abi_value_to_uintN 256

(*
let abi_value_to_intN n evalue =
  Logging.log "abi_value_to_intN code has not been written down";
  12389

let abi_value_to_int8 = abi_value_to_intN 8

let abi_value_to_int16 = abi_value_to_intN 16

let abi_value_to_int24 = abi_value_to_intN 24

let abi_value_to_int32 = abi_value_to_intN 32

let abi_value_to_int40 = abi_value_to_intN 40

let abi_value_to_int48 = abi_value_to_intN 48

let abi_value_to_int56 = abi_value_to_intN 56

let abi_value_to_int64 = abi_value_to_intN 64

let abi_value_to_int = abi_value_to_intN 256
 *)

                      
let abi_value_to_address evalue =
  match evalue with
  | Address_value x -> x
  | _ -> bork "The input is not an address as required"

let abi_value_to_bool evalue =
  match evalue with
  | Bool_value x -> x
  | _ -> bork "The input is not a bool as required"
  
       
  
let big_endian_bytes_of_uint num_bits nat =
  big_endian_bytes_of_nat "uint" num_bits nat nat

let big_endian_bytes_of_int num_bits z =
  let open Z in
  let nat = if sign z >= 0 then z else add z (one lsl num_bits) in
  big_endian_bytes_of_nat "int" num_bits z nat

let abi_intN num_bits z =
  (Int_value (big_endian_bytes_of_int num_bits z), Int num_bits)

let abi_uintN num_bits nat =
  (Uint_value (big_endian_bytes_of_uint num_bits nat), Uint num_bits)

let abi_int8 = abi_intN 8

let abi_int16 = abi_intN 16

let abi_int24 = abi_intN 24

let abi_int32 = abi_intN 32

let abi_int40 = abi_intN 40

let abi_int48 = abi_intN 48

let abi_int56 = abi_intN 56

let abi_int64 = abi_intN 64

(* int is synonym for int256
   don't use make... because bounds checks will fail,
   and any int can be represented in 256 bits
*)
let abi_int z = (Int_value (big_endian_bytes_of_int 256 z), IntDefault)

let abi_uint8 = abi_uintN 8

let abi_uint16 = abi_uintN 16

let abi_uint24 = abi_uintN 24

let abi_uint32 = abi_uintN 32

let abi_uint40 = abi_uintN 40

let abi_uint48 = abi_uintN 48

let abi_uint56 = abi_uintN 56

let abi_uint64 = abi_uintN 64



let abi_value_from_uint64 (evalue : UInt64.t) : abi_value =
  Uint_value (big_endian_bytes_of_uint 64 (UInt64.z_of evalue))

let abi_value_from_revision (evalue : Revision.t) : abi_value =
  Uint_value (big_endian_bytes_of_uint 64 (Revision.z_of evalue))

               
(* uint is synonym for uint256
   don't use make... because bounds checks will fail,
   and any int can be represented in 256 bits
*)
let abi_uint nat = (Uint_value (big_endian_bytes_of_uint 256 nat), UintDefault)

let abi_bytes bytes = (Bytes_value bytes, Bytes (Bytes.length bytes))

let abi_bytes_of_string s = abi_bytes (Bytes.of_string s)

let abi_bytes_dynamic bytes = (Bytes_value bytes, BytesDynamic)

let abi_bytes_dynamic_of_string s = abi_bytes_dynamic (Bytes.of_string s)

let abi_bool b = (Bool_value b, Bool)

let abi_address address = (Address_value address, Address)

let abi_digest = Digest.to_big_endian_bits >> abi_bytes_of_string

let abi_revision revision = abi_uintN Revision.size_in_bits (Revision.z_of revision)

let abi_token_amount amount = abi_uintN TokenAmount.size_in_bits (TokenAmount.z_of amount)

let abi_function_call_of_encoded_call address encoded_call =
  (Function_value (address, encoded_call), Function)

let abi_array_of_abi_values ty abi_typed_vals =
  if not (List.for_all (fun (_, ty') -> ty' = ty) abi_typed_vals) then
    bork "Array elements don't all match specified type" ;
  let len = List.length abi_typed_vals in
  let vals = List.map fst abi_typed_vals in
  (Array_value vals, Array (len, ty))

let abi_array_dynamic_of_abi_values ty abi_typed_vals =
  match fst (abi_array_of_abi_values ty abi_typed_vals) with
  | Array_value vals -> (vals, ArrayDynamic ty)
  | v -> bork "Expected Array value, got %s" (show_abi_value v)

let abi_tuple_of_abi_values abi_typed_vals =
  let vals = List.map fst abi_typed_vals in
  let tys = List.map snd abi_typed_vals in
  (Tuple_value vals, Tuple tys)

(* for Fixed encodings, num is an Int.t or Nat.t which represents num * 10**n *)

let expected_int v =
  bork "Expected Int value, got %s" (show_abi_value v)

let abi_fixed8n n num =
  match fst (abi_int8 num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (8, n))
  | v -> expected_int v

let abi_fixed16n n num =
  match fst (abi_int16 num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (16, n))
  | v -> expected_int v

let abi_fixed24 n num =
  match fst (abi_int24 num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (24, n))
  | v -> expected_int v

let abi_fixed32 n num =
  match fst (abi_int32 num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (32, n))
  | v -> expected_int v

let abi_fixed40 n num =
  match fst (abi_int40 num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (40, n))
  | v -> expected_int v

let abi_fixed48 n num =
  match fst (abi_int48 num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (48, n))
  | v -> expected_int v

let abi_fixed56 n num =
  match fst (abi_int56 num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (56, n))
  | v -> expected_int v

let abi_fixed64 n num =
  match fst (abi_int64 num) with
  | Int_value bytes -> (Fixed_value bytes, Fixed (64, n))
  | v -> expected_int v

let abi_ufixed8n n num =
  match fst (abi_int8 num) with
  | Int_value bytes -> (Ufixed (8, n), Ufixed_value bytes)
  | v -> expected_int v

let abi_ufixed16n n num =
  match fst (abi_int16 num) with
  | Int_value bytes -> (Ufixed (16, n), Ufixed_value bytes)
  | v -> expected_int v

let abi_ufixed24 n num =
  match fst (abi_int24 num) with
  | Int_value bytes -> (Ufixed (24, n), Ufixed_value bytes)
  | v -> expected_int v

let abi_ufixed32 n num =
  match fst (abi_int32 num) with
  | Int_value bytes -> (Ufixed (32, n), Ufixed_value bytes)
  | v -> expected_int v

let abi_ufixed40 n num =
  match fst (abi_int40 num) with
  | Int_value bytes -> (Ufixed (40, n), Ufixed_value bytes)
  | v -> expected_int v

let abi_ufixed48 n num =
  match fst (abi_int48 num) with
  | Int_value bytes -> (Ufixed (48, n), Ufixed_value bytes)
  | v -> expected_int v

let abi_ufixed56 n num =
  match fst (abi_int56 num) with
  | Int_value bytes -> (Ufixed (56, n), Ufixed_value bytes)
  | v -> expected_int v

let abi_ufixed64 n num =
  match fst (abi_int64 num) with
  | Int_value bytes -> (Ufixed (64, n), Ufixed_value bytes)
  | v -> expected_int v

let function_signature function_call =
  (* parameter list is shown as a tuple over the passed types *)
  let tys = List.map snd function_call.parameters in
  let (params : string) = show_type_for_function_selector (Tuple tys) in
  function_call.function_name ^ params

let function_signature_hash = function_signature >> keccak256_string

let function_signature_digest = function_signature >> digest_of_string

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
      bork "have type uint%d, got value with %d bytes" m bytes_len ;
    (* left-pad to 32 bytes *)
    let padding = Bytes.make (32 - bytes_len) '\000' in
    Bytes.cat padding bytes
  | Int_value bytes, Int m ->
    let bytes_len = Bytes.length bytes in
    if m / 8 != bytes_len then
      bork "have type int%d, got value with %d bytes" m bytes_len ;
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
    let num = if b then Nat.one else Nat.zero in
    let uint8_val, uint8_ty = abi_uint8 num in
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
    let len_encoding = big_endian_bytes_of_uint 256 (Nat.of_int len) in
    let total_len = Bytes.length len_encoding + len in
    let extra_padding_len = (~- total_len) land 31 in
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
    let len_encoding = big_endian_bytes_of_uint 256 (Nat.of_int len) in
    let element_encodings = List.map (fun elt -> encode_abi_value elt ty) elts in
    Bytes.concat Bytes.empty (len_encoding :: element_encodings)
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
               let offset_bytes = big_endian_bytes_of_uint 256 (Nat.of_int offset) in
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
    bork "Value to be encoded: %s\nDoes not match its type: %s"
      (show_abi_value v) (show_abi_type ty)

let encode_function_parameters parameters : Bytes.t =
  let param_val, param_ty = abi_tuple_of_abi_values parameters in
  encode_abi_value param_val param_ty

let bytesZero (n : int) : Bytes.t =
  match n with
  | 16 -> big_endian_bytes_of_uint 16 (Nat.of_int 0)
  | 64 -> big_endian_bytes_of_uint 64 (Nat.of_int 0)
  | 256 -> big_endian_bytes_of_uint 256 (Nat.of_int 0)
  | _ -> bork "missing code here"



let get_individual_length (etype: abi_type) : int =
  match etype with
  | Uint _m -> 32
  | Address -> let bytes = Ethereum_util.bytes_of_address Address.zero in 
               let bytes_len = Bytes.length bytes in
               bytes_len
  | Bytes m -> let padding_len = 32 - m in
               if (padding_len < 0) then
                 Logging.log "The array bytes is too long";
               32
  | Bool -> let bytes_val_one = big_endian_bytes_of_uint 8 Nat.one in
            let len = Bytes.length bytes_val_one in
            len
  | _ -> bork "Missing code for this specific type"
  

       
let decode_individual_data (data: Bytes.t) (init_pos: int) (etype: abi_type) : (abi_value*int) =
  Logging.log "Beginning of decode_individual_data init_pos=%i" init_pos;
  match etype with
  | Uint m -> let bytes_zer = bytesZero m in
              (*              Logging.log "decode_individual_data, case 1";*)
              let bytes_len = Bytes.length bytes_zer in
              if m / 8 != bytes_len then
                bork "have type uint%d, got value with %d bytes" m bytes_len ;
              let padding_len = 32 - bytes_len in
              let padding = Bytes.make padding_len '\000' in
              let start_padding_pos = init_pos in
              let _end_padding_pos = init_pos + padding_len in
              let bytes_test = Bytes.sub data start_padding_pos padding_len in
              let start_ret_pos = init_pos + padding_len in
              let end_ret_pos = init_pos + padding_len + bytes_len in
              let bytes_ret = Bytes.sub data start_ret_pos bytes_len in
              let test = Bytes.equal bytes_test padding in
              if (test == false) then
                bork "error in the operation. It should be zero";
              let next_pos = end_ret_pos in
              (Uint_value bytes_ret, next_pos)
  | Address -> let bytes = Ethereum_util.bytes_of_address Address.zero in 
               let bytes_len = Bytes.length bytes in
               let start_pos = init_pos in
               let end_pos = init_pos + bytes_len in
               let data_sub = Bytes.sub data start_pos bytes_len in
               let addr = Ethereum_util.address_of_bytes data_sub in
               (Address_value addr, end_pos)
  | Bytes m -> let padding_len = 32 - m in
               let padding = Bytes.make padding_len '\000' in
               let start_ret_pos = init_pos in
               let end_ret_pos = init_pos + m in
               let bytes_ret = Bytes.sub data start_ret_pos m in
               let start_padding_pos = end_ret_pos in
               let end_padding_pos = end_ret_pos + padding_len in
               let fct_check =
                 if (padding_len>0) then
                   let bytes_padding = Bytes.sub data start_padding_pos padding_len in
                   if (bytes_padding != padding) then
                     true
                   else
                     false
                 else
                   true
               in
               if (fct_check == false) then
                 Logging.log "decode_individual_data, case 3, step 8";
               (Bytes_value bytes_ret, end_padding_pos)
  | Bool -> let bytes_val_one = big_endian_bytes_of_uint 8 Nat.one in
            let bytes_val_zero = big_endian_bytes_of_uint 8 Nat.zero in
            let len = Bytes.length bytes_val_one in
            let start_pos = init_pos in
            let end_pos = init_pos + len in
            let data_sub = Bytes.sub data start_pos len in
            if (bytes_val_one != data_sub) && (bytes_val_zero != data_sub) then
              bork "Error of running";
            let eval = if data_sub == bytes_val_one then true else false in
            (Bool_value eval, end_pos)
  | _ -> bork "Missing code for this specific type"


  
let transcrib_short_int (eval: int) : string =
  let (lchar : string) = "0123456789abcdef" in
  let val2 = eval mod 16 in
  let val1 = (eval - val2) / 16 in
  let str1 = String.sub lchar val1 1 in
  let str2 = String.sub lchar val2 1 in
  let (estr : string) = String.concat "" [str1; str2] in
  estr


       
       
let decode_data (data: Bytes.t) (list_type : abi_type list) : abi_value list =
  let (total_len : int) = Bytes.length data in
  let (list_size : int list) = List.map get_individual_length list_type in
  let (sum_size : int) = sum_int_list list_size in
  let (residue : int) = sum_size mod 32 in
  let (offset : int) = if (residue >0) then
                         32 - residue
                       else
                         0 in
  Logging.log "decode_data, total_len=%i sum_size=%i offset=%i" total_len sum_size offset;
  let _l_data = List.init total_len (fun (i : int) ->
                    let (eval : int) = Char.code (Bytes.get data i) in
                    Logging.log "bytes=%i eval=%i str=%s" i eval (transcrib_short_int eval);
                    i) in
  let padding1 = Bytes.make offset '\000' in
  let padding2 = Bytes.sub data 0 offset in
  let test = Bytes.equal padding1 padding2 in
  Logging.log "test=%B" test;
  if (test == false) then
    Logging.log "We should have padding1 = padding2";
  let (pos : int ref) = ref offset in
  let (list_ret: abi_value list) = List.map (fun etype ->
      Logging.log "decode_data, before call to decode_individual_data";
      let (abi_val, next_pos) = decode_individual_data data !pos etype in
      Logging.log "decode_data, after call to decode_individual_data next_pos=%i" next_pos;
      pos := next_pos;
      abi_val) list_type in
  Logging.log "Now checking the length pos=%i total_len=%i" !pos total_len;
  if (!pos != total_len) then
    Logging.log "The data array size does not match !pos=%i total_len=%i" (!pos) total_len;
  list_ret

       
(* an encoding of the function call is what we pass to Ethereum in a transaction *)
let encode_function_call function_call =
  let (encoded_signature : Bytes.t) = encode_function_signature function_call |> Bytes.of_string in
  let (encoded_params : Bytes.t) = encode_function_parameters function_call.parameters in
  Bytes.cat encoded_signature encoded_params

module Test = struct
  open Hex.Test

  let%test "int64-of-int64-encoding" =
    let ff = Z.of_int 0xff in
    match abi_int64 ff with
    | Int_value bytes, Int 64 ->
      expect_0x_bytes "abi_int64 ff" "0x00000000000000ff" bytes;
      true
    | (v, ty) -> bork "bytes=%s type=%s" (show_abi_value v) (show_abi_type ty)

  (* similar test, but provide int that fits in 56 bits instead of int64 *)
  let%test "int64-of-int-encoding" =
    let ff = Z.of_int 0xff in
    match abi_int56 ff with
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
      Bytes.length bytes = 7 && hi_byte = 0xff && List.for_all (Char.equal '\000') other_bytes
    | _ -> false

  let%test "contract-call-encoding-1" =
    (* first example from ABI spec *)
    let param1 = abi_uint32 (Nat.of_int 69) in
    let param2 = abi_bool true in
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
    let param2 = abi_bool true in
    let param3_array = List.map (fun n -> fst (abi_uint (Nat.of_int n))) [1; 2; 3] in
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
    let param1 = abi_uint (Nat.of_int 0x123) in
    let param2 =
      let num1, ty1 = abi_uint32 (Nat.of_int 0x456) in
      let num2, ty2 = abi_uint32 (Nat.of_int 0x789) in
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
