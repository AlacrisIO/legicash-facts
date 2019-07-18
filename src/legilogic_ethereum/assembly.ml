open Legilogic_lib
open Lib
open Signing

(* In the future, segments can be nested, and
   offset would be relative to a subsegment of a segment,
   to be fully resolved when the segment size is finalized.
   (Also, a non-embedded front-end syntax.)

   For now, everything is fixed size and we don't compute
   label offsets much less optimize segment layout:
   instead we merely *check* that labels are used properly
   and the formulas match.
 *)
type offset = int [@@deriving show, yojson, rlp]

(* 24576, limit set by EIP 170 https://github.com/ethereum/EIPs/blob/master/EIPS/eip-170.md *)
let max_segment_size = 0x6000

(* TODO: use pure functional arrays?
module Segment = struct
  type state =
    | Bytes of Bytes.t
    | Buffer of Butter.t
    | Concat of Segment list
  type t = { id : int; name : string ; state : mutable state ; offset : offset }
  let create asm =
    let segment =
      { id=new_id
      ; state=Buffer (Buffer.create max_segment_size)
      ; offset=current_offset asm } in
    register_segment asm segment;
    segment
end
*)

type expr =
  | Offset of offset
  | Label of string
  | Add of expr * expr
  | Sub of expr * expr [@@deriving show, yojson, rlp]

module Fixup = struct
  (* for now, manually set by the user, in the future, can be dynamic in a range? *)
  type size = int [@@deriving show, yojson, rlp]
  type t = expr * size [@@deriving show, yojson, rlp]
end

module Label = struct
  type t = { offset : offset option
           ; fixups : offset list } [@@deriving show, yojson, rlp]
end

module Assembler = struct
  type t = { buffer : Buffer.t (* in the future, a Segment.t *)
           ; labels : (string, Label.t) Hashtbl.t
           ; fixups : (int, Fixup.t) Hashtbl.t }
  let create () = { buffer = Buffer.create max_segment_size
                  ; labels = Hashtbl.create 100
                  ; fixups = Hashtbl.create 200 }
end

(* TODO: have chunks of code of constant or unknown but bounded length,
   labels, fixups, displacements, etc.; compile-time merging of constant stuff(?) *)
type directive = Assembler.t -> unit

exception Label_already_defined

let rec eval_expr labels = function
  | Offset o -> o
  | Label l -> Option.get (Hashtbl.find labels l).Label.offset
  | Add (x, y) -> (eval_expr labels x) + (eval_expr labels y)
  | Sub (x, y) -> (eval_expr labels x) - (eval_expr labels y)

let char x asm = Buffer.add_char asm.Assembler.buffer x
let byte x = char (Char.chr x)
let string s asm = Buffer.add_string asm.Assembler.buffer s
let rec int size value asm =
  if size > 0 then
    (byte ((value lsr (8 * (size - 1))) land 255) asm;
     int (size - 1) value asm)
let current_offset asm = Buffer.length asm.Assembler.buffer

let check_byte asm offset value msg =
  if not (Buffer.nth asm.Assembler.buffer offset = Char.chr value) then
    raise (Internal_error (msg ()))

let rec get_buffer_value buffer offset size =
  if size = 0 then 0 else
    let v = Char.code (Buffer.nth buffer offset) in
    if size = 1 then v else 256 * v + (get_buffer_value buffer (offset + 1) (size - 1))

let rec check_bytes asm n offset value msg =
  if n > 0 then
    (check_byte asm offset ((value lsr (8 * (n - 1))) land 255) msg;
     check_bytes asm (n - 1) (offset + 1) value msg)

let check_fixup asm offset (expr, size) =
  let value = eval_expr asm.Assembler.labels expr in
  if not (value >= 0 && Z.(numbits (of_int value)) < (8 * size)) then
    bork "fixup @ %d %s size %d has incorrect computed value %d"
      offset (show_expr expr) size value;
  check_bytes asm size offset value
    (fun _ -> Printf.sprintf "fixup @ %d %s size %d computed value %d doesn't match given value %d"
                offset (show_expr expr) size value (get_buffer_value asm.Assembler.buffer offset size));
  Hashtbl.remove asm.Assembler.fixups offset

let examine_fixup asm offset (expr, size) =
  try check_fixup asm offset (expr, size) with _ -> () (* TODO: have useful error messages *)

let fixup size expr value asm =
  let offset = Buffer.length asm.Assembler.buffer in
  int size value asm;
  Hashtbl.add asm.Assembler.fixups offset (expr, size)

let label l asm =
  match Hashtbl.find_opt asm.Assembler.labels l with
    | Some { offset= Some _ } ->
       raise Label_already_defined
    | Some { fixups } ->
       let lbl = Label.{ offset=Some (current_offset asm); fixups } in
       Hashtbl.replace asm.labels l lbl
    | None ->
       let lbl = Label.{ offset=Some (current_offset asm); fixups= [] } in
       Hashtbl.add asm.labels l lbl

let eSTOP = byte 0x00 (* Halts execution	-	0 *)
let eADD = byte 0x01 (* Addition operation	-	3 *)
let eMUL = byte 0x02 (* Multiplication operation	-	5 *)
let eSUB = byte 0x03 (* Subtraction operation	-	3 *)
let eDIV = byte 0x04 (* Integer division operation	-	5 *)
let eSDIV = byte 0x05 (* Signed integer division operation (truncated)	-	5 *)
let eMOD = byte 0x06 (* Modulo remainder operation	-	5 *)
let eSMOD = byte 0x07 (* Signed modulo remainder operation	-	5 *)
let eADDMOD = byte 0x08 (* Modulo addition operation	-	8 *)
let eMULMOD = byte 0x09 (* Modulo multiplication operation	-	8 *)
let eEXP = byte 0x0a (* Exponential operation	-	10* *)
let eSIGNEXTEND = byte 0x0b (* Extend length of two's complement signed integer	-	5 *)
(* 0x0c - 0x0f	Unused	Unused	-	*)
let eLT = byte 0x10 (* Less-than comparison	-	3 *)
let eGT = byte 0x11 (* Greater-than comparison	-	3 *)
let eSLT = byte 0x12 (* Signed less-than comparison	-	3 *)
let eSGT = byte 0x13 (* Signed greater-than comparison	-	3 *)
let eEQ = byte 0x14 (* Equality comparison	-	3 *)
let eISZERO = byte 0x15 (* Simple not operator	-	3 *)
let eAND = byte 0x16 (* Bitwise AND operation	-	3 *)
let eOR = byte 0x17 (* Bitwise OR operation	-	3 *)
let eXOR = byte 0x18 (* Bitwise XOR operation	-	3 *)
let eNOT = byte 0x19 (* Bitwise NOT operation	-	3 *)
let eBYTE = byte 0x1a (* Retrieve single byte from word	-	3 *)
let eSHA3 = byte 0x20 (* Compute Keccak-256 hash	-	30* *)
(* 0x21 - 0x2f	Unused	Unused		*)
let eADDRESS = byte 0x30 (* Get address of currently executing account	-	2 *)
let eBALANCE = byte 0x31 (* Get balance of the given account	-	400 *)
let eORIGIN = byte 0x32 (* Get execution origination address	-	2 *)
let eCALLER = byte 0x33 (* Get caller address	-	2 *)
let eCALLVALUE = byte 0x34 (* Get deposited value by the instruction/transaction responsible for this execution	-	2 *)
let eCALLDATALOAD = byte 0x35 (* Get input data of current environment	-	3 *)
let eCALLDATASIZE = byte 0x36 (* Get size of input data in current environment	-	2* *)
let eCALLDATACOPY = byte 0x37 (* Copy input data in current environment to memory	-	3 *)
let eCODESIZE = byte 0x38 (* Get size of code running in current environment	-	2 *)
let eCODECOPY = byte 0x39 (* Copy code running in current environment to memory	-	3* *)
let eGASPRICE = byte 0x3a (* Get price of gas in current environment	-	2 *)
let eEXTCODESIZE = byte 0x3b (* Get size of an account's code	-	700 *)
let eEXTCODECOPY = byte 0x3c (* Copy an account's code to memory	-	700* *)
let eRETURNDATASIZE = byte 0x3d (* Pushes the size of the return data buffer onto the stack	EIP 211	2 *)
let eRETURNDATACOPY = byte 0x3e (* Copies data from the return data buffer to memory	EIP 211	3 *)
let eUNUSED = byte 0x3f (* -		 *)
let eBLOCKHASH = byte 0x40 (* Get the hash of one of the 256 most recent complete blocks	-	20 *)
let eCOINBASE = byte 0x41 (* Get the block's beneficiary address	-	2 *)
let eTIMESTAMP = byte 0x42 (* Get the block's timestamp	-	2 *)
let eNUMBER = byte 0x43 (* Get the block's number	-	2 *)
let eDIFFICULTY = byte 0x44 (* Get the block's difficulty	-	2 *)
let eGASLIMIT = byte 0x45 (* Get the block's gas limit	-	2 *)
(* 0x46 - 0x4f	Unused	-		*)
let ePOP = byte 0x50 (* Remove word from stack	-	2 *)
let eMLOAD = byte 0x51 (* Load word from memory	-	3* *)
let eMSTORE = byte 0x52 (* Save word to memory	-	3* *)
let eMSTORE8 = byte 0x53 (* Save byte to memory	-	3 *)
let eSLOAD = byte 0x54 (* Load word from storage	-	200 *)
let eSSTORE = byte 0x55 (* Save word to storage	-	20000** *)
let eJUMP = byte 0x56 (* Alter the program counter	-	8 *)
let eJUMPI = byte 0x57 (* Conditionally alter the program counter	-	10 *)
let eGETPC = byte 0x58 (* Get the value of the program counter prior to the increment	-	2 *)
let eMSIZE = byte 0x59 (* Get the size of active memory in bytes	-	2 *)
let eGAS = byte 0x5a (* Get the amount of available gas, including the corresponding reduction the amount of available gas	-	2 *)
let eJUMPDEST = byte 0x5b (* Mark a valid destination for jumps	-	1 *)
(* 0x5c - 0x5f	Unused	-		*)
let ePUSH1 = byte 0x60 (* Place 1 byte item on stack	-	3 *)
let ePUSH2 = byte 0x61 (* Place 2-byte item on stack	-	3 *)
let ePUSH3 = byte 0x62 (* Place 3-byte item on stack	-	3 *)
let ePUSH4 = byte 0x63 (* Place 4-byte item on stack	-	3 *)
let ePUSH5 = byte 0x64 (* Place 5-byte item on stack	-	3 *)
let ePUSH6 = byte 0x65 (* Place 6-byte item on stack	-	3 *)
let ePUSH7 = byte 0x66 (* Place 7-byte item on stack	-	3 *)
let ePUSH8 = byte 0x67 (* Place 8-byte item on stack	-	3 *)
let ePUSH9 = byte 0x68 (* Place 9-byte item on stack	-	3 *)
let ePUSH10 = byte 0x69 (* Place 10-byte item on stack	-	3 *)
let ePUSH11 = byte 0x6a (* Place 11-byte item on stack	-	3 *)
let ePUSH12 = byte 0x6b (* Place 12-byte item on stack	-	3 *)
let ePUSH13 = byte 0x6c (* Place 13-byte item on stack	-	3 *)
let ePUSH14 = byte 0x6d (* Place 14-byte item on stack	-	3 *)
let ePUSH15 = byte 0x6e (* Place 15-byte item on stack	-	3 *)
let ePUSH16 = byte 0x6f (* Place 16-byte item on stack	-	3 *)
let ePUSH17 = byte 0x70 (* Place 17-byte item on stack	-	3 *)
let ePUSH18 = byte 0x71 (* Place 18-byte item on stack	-	3 *)
let ePUSH19 = byte 0x72 (* Place 19-byte item on stack	-	3 *)
let ePUSH20 = byte 0x73 (* Place 20-byte item on stack	-	3 *)
let ePUSH21 = byte 0x74 (* Place 21-byte item on stack	-	3 *)
let ePUSH22 = byte 0x75 (* Place 22-byte item on stack	-	3 *)
let ePUSH23 = byte 0x76 (* Place 23-byte item on stack	-	3 *)
let ePUSH24 = byte 0x77 (* Place 24-byte item on stack	-	3 *)
let ePUSH25 = byte 0x78 (* Place 25-byte item on stack	-	3 *)
let ePUSH26 = byte 0x79 (* Place 26-byte item on stack	-	3 *)
let ePUSH27 = byte 0x7a (* Place 27-byte item on stack	-	3 *)
let ePUSH28 = byte 0x7b (* Place 28-byte item on stack	-	3 *)
let ePUSH29 = byte 0x7c (* Place 29-byte item on stack	-	3 *)
let ePUSH30 = byte 0x7d (* Place 30-byte item on stack	-	3 *)
let ePUSH31 = byte 0x7e (* Place 31-byte item on stack	-	3 *)
let ePUSH32 = byte 0x7f (* Place 32-byte (full word) item on stack	-	3 *)
let eDUP1 = byte 0x80 (* Duplicate 1st stack item	-	3 *)
let eDUP2 = byte 0x81 (* Duplicate 2nd stack item	-	3 *)
let eDUP3 = byte 0x82 (* Duplicate 3rd stack item	-	3 *)
let eDUP4 = byte 0x83 (* Duplicate 4th stack item	-	3 *)
let eDUP5 = byte 0x84 (* Duplicate 5th stack item	-	3 *)
let eDUP6 = byte 0x85 (* Duplicate 6th stack item	-	3 *)
let eDUP7 = byte 0x86 (* Duplicate 7th stack item	-	3 *)
let eDUP8 = byte 0x87 (* Duplicate 8th stack item	-	3 *)
let eDUP9 = byte 0x88 (* Duplicate 9th stack item	-	3 *)
let eDUP10 = byte 0x89 (* Duplicate 10th stack item	-	3 *)
let eDUP11 = byte 0x8a (* Duplicate 11th stack item	-	3 *)
let eDUP12 = byte 0x8b (* Duplicate 12th stack item	-	3 *)
let eDUP13 = byte 0x8c (* Duplicate 13th stack item	-	3 *)
let eDUP14 = byte 0x8d (* Duplicate 14th stack item	-	3 *)
let eDUP15 = byte 0x8e (* Duplicate 15th stack item	-	3 *)
let eDUP16 = byte 0x8f (* Duplicate 16th stack item	-	3 *)
let eSWAP1 = byte 0x90 (* Exchange 1st and 2nd stack items	-	3 *)
let eSWAP2 = byte 0x91 (* Exchange 1st and 3rd stack items	-	3 *)
let eSWAP3 = byte 0x92 (* Exchange 1st and 4th stack items	-	3 *)
let eSWAP4 = byte 0x93 (* Exchange 1st and 5th stack items	-	3 *)
let eSWAP5 = byte 0x94 (* Exchange 1st and 6th stack items	-	3 *)
let eSWAP6 = byte 0x95 (* Exchange 1st and 7th stack items	-	3 *)
let eSWAP7 = byte 0x96 (* Exchange 1st and 8th stack items	-	3 *)
let eSWAP8 = byte 0x97 (* Exchange 1st and 9th stack items	-	3 *)
let eSWAP9 = byte 0x98 (* Exchange 1st and 10th stack items	-	3 *)
let eSWAP10 = byte 0x99 (* Exchange 1st and 11th stack items	-	3 *)
let eSWAP11 = byte 0x9a (* Exchange 1st and 12th stack items	-	3 *)
let eSWAP12 = byte 0x9b (* Exchange 1st and 13th stack items	-	3 *)
let eSWAP13 = byte 0x9c (* Exchange 1st and 14th stack items	-	3 *)
let eSWAP14 = byte 0x9d (* Exchange 1st and 15th stack items	-	3 *)
let eSWAP15 = byte 0x9e (* Exchange 1st and 16th stack items	-	3 *)
let eSWAP16 = byte 0x9f (* Exchange 1st and 17th stack items	-	3 *)
let eLOG0 = byte 0xa0 (* Append log record with no topics	-	375 *)
let eLOG1 = byte 0xa1 (* Append log record with one topic	-	750 *)
let eLOG2 = byte 0xa2 (* Append log record with two topics	-	1125 *)
let eLOG3 = byte 0xa3 (* Append log record with three topics	-	1500 *)
let eLOG4 = byte 0xa4 (* Append log record with four topics	-	1875 *)
(* 0xa5 - 0xaf	Unused	-		*)
let eJUMPTO = byte 0xb0 (* Tentative libevmasm has different numbers	EIP 615	 *)
let eJUMPIF = byte 0xb1 (* Tentative	EIP 615	 *)
let eJUMPSUB = byte 0xb2 (* Tentative	EIP 615	 *)
let eJUMPSUBV = byte 0xb4 (* Tentative	EIP 615	 *)
let eBEGINSUB = byte 0xb5 (* Tentative	EIP 615	 *)
let eBEGINDATA = byte 0xb6 (* Tentative	EIP 615	 *)
let eRETURNSUB = byte 0xb8 (* Tentative	EIP 615	 *)
let ePUTLOCAL = byte 0xb9 (* Tentative	EIP 615	 *)
let eGETLOCAL = byte 0xba (* Tentative	EIP 615	 *)
(* 0xbb - 0xe0	Unused	-		*)
let eSLOADBYTES = byte 0xe1 (* Only referenced in pyethereum	-	- *)
let eSSTOREBYTES = byte 0xe2 (* Only referenced in pyethereum	-	- *)
let eSSIZE = byte 0xe3 (* Only referenced in pyethereum	-	- *)
(* 0xe4 - 0xef	Unused	-		*)
let eCREATE = byte 0xf0 (* Create a new account with associated code	-	32000 *)
let eCALL = byte 0xf1 (* Message-call into an account	-	Complicated *)
let eCALLCODE = byte 0xf2 (* Message-call into this account with alternative account's code	-	Complicated *)
let eRETURN = byte 0xf3 (* Halt execution returning output data	-	0 *)
let eDELEGATECALL = byte 0xf4 (* Message-call into this account with an alternative account's code, but persisting into this account with an alternative account's code	-	Complicated *)
let eCALLBLACKBOX = byte 0xf5 (* -	-	 *)
(* 0xf6 - 0xf9	Unused	-	-	*)
let eSTATICCALL = byte 0xfa (* Similar to CALL, but does not modify state	-	40 *)
let eCREATE2 = byte 0xfb (* Create a new account and set creation address to sha3(sender + sha3(init code)) % 2**160	-	 *)
let eTXEXECGAS = byte 0xfc (* Not in yellow paper FIXME	-	- *)
let eREVERT = byte 0xfd (* Stop execution and revert state changes, without consuming all provided gas and providing a reason	-	0 *)
let eINVALID = byte 0xfe (* Designated invalid instruction	-	0 *)
let eSELFDESTRUCT = byte 0xff (* Halt execution and register account for later deletion	-	5000* *)

let jumpdest l asm =
  label l asm; eJUMPDEST asm

let pushlabel1 l n asm =
  ePUSH1 asm; fixup 1 (Label l) n asm

let pushlabel2 l n asm =
  ePUSH2 asm; fixup 2 (Label l) n asm

let jump1 l n asm =
  pushlabel1 l n asm; eJUMP asm

let jump2 l n asm =
  pushlabel1 2 n asm; eJUMP asm

let jumpi1 l n asm =
  pushlabel1 l n asm; eJUMPI asm

let jumpi2 l n asm =
  pushlabel1 2 n asm; eJUMPI asm

let push_address address asm =
  ePUSH20 asm; string (Address.to_big_endian_bits address) asm

let rec push_z z asm =
  let s = Z.sign z in
  if (s = -1) && (Z.numbits z < 240) then
    (push_z (Z.lognot z) asm; eNOT asm)
  else if s = 0 then
    (ePUSH1 asm; byte 0 asm)
  else
    let bits = string_reverse Z.(to_bits (extract z 0 256)) in
    (byte (0x5f + (String.length bits)) asm; string bits asm)

let rec push_int n asm =
  if n < 0 then
    (push_int (lnot n) asm; eNOT asm)
  else if n < 256 then
    (ePUSH1 asm; byte n asm)
  else
    push_z (Z.of_int n) asm

let push_address x asm =
  push_z (Address.z_of x) asm

type program = directive list

let assemble_directives buffer program =
  List.iter (fun directive -> directive buffer) program

let assemble program =
  let asm = Assembler.create () in
  assemble_directives asm program;
  Hashtbl.iter (check_fixup asm) asm.Assembler.fixups;
  Buffer.contents asm.Assembler.buffer
