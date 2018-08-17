open Lib

type yojson = Yojson.Safe.json
let string_of_yojson y = Yojson.Safe.to_string y
let yojson_of_string s = Yojson.Safe.from_string s

module YoJson = struct
  include Yojson.Safe.Util
  let mem key yojson = List.mem key (keys yojson)
end

type 'a to_yojson = 'a -> yojson
type 'a of_yojson = yojson -> ('a, string) result
type 'a of_yojson_exn = yojson -> 'a
type 'a yojsoning = { to_yojson: 'a to_yojson; of_yojson: 'a of_yojson }

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

let of_yojson_string_exn_of_of_yojson_exn of_yojson_exn s =
  of_yojson_exn (yojson_of_string s)

let to_yojson_map f to_yojson x = x |> f |> to_yojson
let of_yojson_map f of_yojson y = y |> of_yojson |> Result.map f
let yojsoning_map f g yojsoning =
  { to_yojson=to_yojson_map f yojsoning.to_yojson
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

let list_of_yojson of_yojson = function
  | `List l -> Result.list_map of_yojson l
  | _ -> Error "bad json list"

let list_of_yojson_exn of_yojson_exn = function
  | `List l -> (List.map of_yojson_exn l)
  | _ -> Yojson.json_error "bad json list"

let list_yojsoning yojsoning =
  { to_yojson=list_to_yojson yojsoning.to_yojson
  ; of_yojson=list_of_yojson yojsoning.of_yojson }

let string_yojsoning =
  { to_yojson = (fun x -> `String x)
  ; of_yojson = function
      | `String x -> Ok x
      | _ -> Error "not a json string" }

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
