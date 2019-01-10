(* version.ml -- suport for semantic versioning *)

type status = Alpha of int | Beta of int | Release

type version = {major_version: int; minor_version: int; patch_version: int; status: status}

let current_version = {major_version= 0; minor_version= 1; patch_version= 0; status= Alpha 0}

let get_version_string () =
  let suffix =
    match current_version.status with
    | Alpha n -> "-alpha." ^ string_of_int n
    | Beta n -> "-beta." ^ string_of_int n
    | Release -> ""
  in
  Format.sprintf "%d.%d.%d%s" current_version.major_version current_version.minor_version
    current_version.patch_version suffix
