(** TODO: add lots of tests! *)

type t = int64

let zero = Int64.zero

let one = Int64.one

let minus_one = Int64.minus_one

let f2xx63 = 2. ** 63.

let u2xx63 = Int64.min_int

let neg = Int64.neg

let add = Int64.add

let sub = Int64.sub

let compare x y = Int64.compare (add x u2xx63) (add y u2xx63)

let equal = Int64.equal

let logand = Int64.logand

let logor = Int64.logor

let logxor = Int64.logxor

let lognot = Int64.lognot

let shift_left = Int64.shift_left

let shift_right_logical = Int64.shift_right_logical

let shift_right = shift_right_logical

let shift_right_arithmetic = Int64.shift_right

let lo32mask = Int64.of_string "0xFFFFFFFF"

let bits_of_float = Int64.bits_of_float

let of_int64 x = x

let to_int64 x = x

let float_of_bits = Int64.float_of_bits

let lo32 x = logand x lo32mask

let hi32 x = shift_right x 32

(** TODO: do only one 63*63 bit multiplication, then masking + additions *)
let mul x y =
  let xlo, xhi, ylo, yhi = (lo32 x, hi32 x, lo32 y, hi32 y) in
  add (Int64.mul xlo ylo)
    (shift_left (add (Int64.mul xlo yhi) (Int64.mul xhi ylo)) 32)


let succ = Int64.succ

let pred = Int64.pred

let abs x = x

let max_int = minus_one

let min_int = zero

let of_int = Int64.of_int

let to_int = Int64.to_int

let of_int32 = Int64.of_int32

let to_int32 = Int64.to_int32

let of_nativeint = Int64.of_nativeint

let to_nativeint = Int64.to_nativeint

let rec div x y =
  let ysign = Int64.compare y zero in
  if ysign = 0 then raise Division_by_zero
  else if ysign > 0 then
    let xsign = Int64.compare x zero in
    if xsign > 0 then Int64.div x y
    else if xsign = 0 then zero
    else
      let xh = shift_right x 1 in
      let qh = div xh y in
      let x1 = sub x (mul qh y) in
      let q0 = shift_left qh 1 in
      if compare x1 y >= 0 then add q0 one else q0
  else if (* ysign < 0 *)
          compare x y >= 0 then one
  else zero


let is_odd x = equal (logand x one) one

(** Same as: sub x (mul (div x y) y) *)
let rem x y =
  let ysign = Int64.compare y zero in
  if ysign = 0 then raise Division_by_zero
  else if ysign > 0 then
    let xsign = Int64.compare x zero in
    if xsign > 0 then Int64.rem x y
    else if xsign = 0 then zero
    else
      let xh = shift_right x 1 in
      let rh = rem xh y in
      let r1 = add (shift_left rh 1) (logand x one) in
      if compare r1 y >= 0 then sub r1 y else r1
  else if (* ysign < 0 *)
          compare x y >= 0 then sub x y
  else x


let of_float f =
  if f < f2xx63 then Int64.of_float f
  else add (Int64.of_float (f -. f2xx63)) u2xx63


let to_float x =
  if Int64.compare x zero >= 0 then Int64.to_float x
  else Int64.to_float (sub x u2xx63) +. f2xx63


let is_string_prefix s prefix =
  String.length prefix <= String.length s
  && String.equal prefix (String.sub s 0 (String.length prefix))


let of_string_helper f g s =
  if is_string_prefix "-" s then g ()
  else if is_string_prefix "0x" s || is_string_prefix "0o" s
          || is_string_prefix "0b" s || is_string_prefix "0u" s
  then f s
  else f ("0u" ^ s)


let of_string =
  of_string_helper Int64.of_string (fun () ->
      raise (Failure "UInt64.of_string") )


let of_string_opt = of_string_helper Int64.of_string_opt (fun () -> None)

let ten = Int64.of_string "10"

let to_string x =
  let q = div x ten in
  let rs = Int64.to_string (rem x ten) in
  if equal q zero then rs else Int64.to_string q ^ rs


let is_add_carry x y = compare x (lognot y) > 0
