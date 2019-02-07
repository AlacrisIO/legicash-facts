open Lib

type yojson = Yojson.Safe.t
let string_of_yojson y = Yojson.Safe.to_string y
let yojson_of_string s = Yojson.Safe.from_string s

let yojson_of_file fn = Yojson.Safe.from_file fn

let pp_yojson formatter x = Format.fprintf formatter "%s" (string_of_yojson x)
let show_yojson x = Format.asprintf "%a" pp_yojson x

let yojson_string s = `String s
let yojson_list l = `List l

module YoJson = struct
  include Yojson.Safe.Util
  let mem key yojson =
    match yojson with
    | `Assoc _ -> List.mem key (keys yojson)
    | _ -> false
end

type 'a to_yojson = 'a -> yojson
type 'a of_yojson = yojson -> ('a, string) result
type 'a of_yojson_exn = yojson -> 'a
type 'a yojsoning = { to_yojson: 'a to_yojson; of_yojson: 'a of_yojson }

let yojson_to_yojson = identity
let yojson_of_yojson x = Ok x
let yojson_yojsoning = {to_yojson=yojson_to_yojson; of_yojson=yojson_of_yojson}

let of_yojson_exn_of_of_yojson of_yojson y =
  match (of_yojson y) with
  | Ok x -> x
  | Error s -> Yojson.json_error s

let of_yojson_of_of_yojson_exn of_yojson_exn y =
  try Ok (of_yojson_exn y) with
  | Internal_error x -> Error ("Internal_error " ^ x)
  | Yojson.Json_error x -> Error ("Json_error " ^ x)

let to_yojson_string_of_to_yojson to_yojson x =
  x |> to_yojson |> string_of_yojson

let of_yojson_string_exn_of_of_yojson_exn of_yojson_exn x =
  x |> yojson_of_string |> of_yojson_exn

let of_yojson_string_exn_of_of_yojson x =
  x |> of_yojson_exn_of_of_yojson |> of_yojson_string_exn_of_of_yojson_exn

let to_yojson_map f to_yojson x = x |> f |> to_yojson
let of_yojson_map f of_yojson y = y |> of_yojson |> Result.map f
let yojsoning_map f g yojsoning =
  { to_yojson=to_yojson_map f yojsoning.to_yojson
  (* TODO: [of_yojson] should handle errors, and [g] should either return either
     a [Result], or we should catch the exception and return an [Error]. *)
  ; of_yojson=of_yojson_map g yojsoning.of_yojson }

let option_to_yojson to_yojson = function
  | None -> `Null
  | Some x -> to_yojson x

let option_of_yojson of_yojson = function
  | `Null -> Ok None
  | x -> Result.map (fun x -> Some x) (of_yojson x)

let option_of_yojson_exn of_yojson_exn = function
  | `Null -> None
  | x -> Some (of_yojson_exn x)

let option_yojsoning yojsoning =
  { to_yojson=option_to_yojson yojsoning.to_yojson
  ; of_yojson=option_of_yojson yojsoning.of_yojson }

let list_to_yojson to_yojson l = `List (List.map to_yojson l)

(** Return the type of [y], as a string, for JSON-parsing error messages **)
let yojson_type_string (y : yojson) : string =
  match y with
  | `Null     -> "null"   | `Int _   -> "int"   | `Float _   -> "float"
  | `String _ -> "string" | `Assoc _ -> "assoc" | `List _    -> "list"
  | `Tuple _  -> "tuple"  | `Bool _  -> "bool"  | `Variant _ -> "variant"
  | `Intlit _ -> "intlit"

let yojson_error_string (s : string) (x : yojson) : string =
  Printf.sprintf "%s: %s is a %s " s (string_of_yojson x) (yojson_type_string x)

let list_of_yojson of_yojson = function
  | `List l -> Result.list_map of_yojson l
  | _ as x -> Error (yojson_error_string "bad json list" x)

let list_of_yojson_exn of_yojson_exn = function
  | `List l -> (List.map of_yojson_exn l)
  | _ as x -> Yojson.json_error @@ yojson_error_string "bad json list" x

let list_yojsoning yojsoning =
  { to_yojson=list_to_yojson yojsoning.to_yojson
  ; of_yojson=list_of_yojson yojsoning.of_yojson }

let string_yojsoning =
  { to_yojson = (fun x -> `String x)
  ; of_yojson = function
      | `String x -> Ok x
      | _ as x -> Error (yojson_error_string "not a json string" x)
  }

let string_of_char = String.make 1
let char_of_string s =
  if String.length s = 1 then
    String.get s 0
  else
    bork "string isn't of length 1"
let char_yojsoning = yojsoning_map string_of_char char_of_string string_yojsoning

module type PreYojsonableS = sig
  type t
  val yojsoning : t yojsoning
end

module type YojsonableS = sig
  include PreYojsonableS
  val to_yojson : t to_yojson
  val of_yojson : t of_yojson
  val of_yojson_exn : t of_yojson_exn
  val to_yojson_string : t -> string
  val of_yojson_string_exn : string -> t
end

module Yojsonable (P : PreYojsonableS) = struct
  include P
  let to_yojson = yojsoning.to_yojson
  let of_yojson = yojsoning.of_yojson
  let of_yojson_exn = of_yojson_exn_of_of_yojson yojsoning.of_yojson
  let to_yojson_string = to_yojson_string_of_to_yojson to_yojson
  let of_yojson_string_exn = of_yojson_string_exn_of_of_yojson_exn of_yojson_exn
end

module NotYojsonable (T : TypeS) = struct
  type t = T.t
  let yojsoning = {to_yojson=bottom;of_yojson=bottom}
  let to_yojson = bottom
  let of_yojson = bottom
  let of_yojson_exn = bottom
  let to_yojson_string = bottom
  let of_yojson_string_exn = bottom
end

let string_0x_yojsoning =
  yojsoning_map Hex.unparse_0x_data Hex.parse_0x_data string_yojsoning

let bytes_yojsoning =
  yojsoning_map Bytes.to_string Bytes.of_string string_0x_yojsoning

module Bytes = struct
  include Bytes
  include (Yojsonable(struct
             type t = Bytes.t
             let yojsoning = bytes_yojsoning
           end) : YojsonableS with type t := t)
  let pp formatter x = Format.fprintf formatter "(parse_0x_bytes %S)" (Hex.unparse_0x_bytes x)
  let show x = Format.asprintf "%a" pp x
end
