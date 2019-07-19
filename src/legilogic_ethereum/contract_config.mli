open Legilogic_lib
open Action
open Signing
open Persisting
open Types

module ContractConfig : sig
  type t =
    { contract_address : Address.t
    ; code_hash : Digest.t
    ; creation_hash : Digest.t
    ; creation_block : Revision.t
    } [@@deriving lens { prefix = true }, show, eq]
  include PersistableS with type t := t
end

val contract_config_of_creation_hash : Digest.t -> ContractConfig.t Lwt_exn.t

val contract_config_of_config_file : string -> ContractConfig.t

val contract_config_to_file : string -> ContractConfig.t -> unit Lwt_exn.t

val contract_config_to_config_file : string -> ContractConfig.t -> unit Lwt_exn.t

val contract_config_of_db : string -> ContractConfig.t

val contract_config_to_db : string -> ContractConfig.t -> unit Lwt_exn.t

val verify_contract_config : ContractConfig.t -> ContractConfig.t Lwt_exn.t

val ensure_contract : ('a -> ContractConfig.t) -> ('a -> ContractConfig.t -> unit Lwt_exn.t) -> 'a
                      -> (unit -> Digest.t Lwt_exn.t) -> ContractConfig.t Lwt_exn.t

val ensure_contract_of_config_file :
  string -> (unit -> Digest.t Lwt_exn.t) -> ContractConfig.t Lwt_exn.t

val ensure_contract_of_db :
  string -> (unit -> Digest.t Lwt_exn.t) -> ContractConfig.t Lwt_exn.t
