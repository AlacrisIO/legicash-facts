(* version.mli -- support for semantic versioning *)

type status = Alpha of int | Beta of int | Release

type version = {major_version: int; minor_version: int; patch_version: int; status: status}

val current_version : version
(** record with semantic version information *)

val get_version_string : unit -> string
(** formatted string with current semantic version information *)
