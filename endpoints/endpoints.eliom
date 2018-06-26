[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
]

open Lwt

(*
module Endpoints_app =
  Eliom_registration.App (
    struct
      let application_name = "endpoints"
      let global_data_path = None
    end)

let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let () =
  Endpoints_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"endpoints"
           ~css:[["css";"endpoints.css"]]
           Html.F.(body [
             h1 [pcdata "Welcome from Eliom's distillery!"];
           ])))
*)


(**** Data types ****)

type coordinates = {
  latitude : float;
  longitude : float;
} [@@deriving yojson]

type location = {
  description : string option;
  coordinates : coordinates;
} [@@deriving yojson]

(* List of pairs (identifier * location) *)
type locations =
  (string * location) list
[@@deriving yojson]

type error = {
  error_message : string;
} [@@deriving yojson]

let db : location Ocsipersist.table Lwt.t =
  Ocsipersist.open_table "locations"


(**** Services ****)

let path = Eliom_service.Path [""]

let get_params =
  Eliom_parameter.(suffix (neopt (string "id")))

let read_service =
  Eliom_service.create
    ~path
    ~meth:(Eliom_service.Get get_params)
    ()

let create_service =
  Eliom_service.create
    ~path
    ~meth:(Eliom_service.Post (get_params, Eliom_parameter.raw_post_data))
    ()

let update_service =
  Eliom_service.create
    ~path
    ~meth:(Eliom_service.Put get_params)
    ()

let delete_service =
  Eliom_service.create
    ~path
    ~meth:(Eliom_service.Delete get_params)
    ()

(**** Handler helpers ****)

let json_mime_type = "application/json"

let send_json ~code json =
  Eliom_registration.String.send ~code (json, json_mime_type)

let send_error ~code error_message =
  let json = Yojson.Safe.to_string (error_to_yojson {error_message}) in
  send_json ~code json

let send_success () =
  Eliom_registration.String.send ~code:200 ("", "")

let check_content_type ~mime_type content_type =
  match content_type with
  | Some ((type_, subtype), _)
    when (type_ ^ "/" ^ subtype) = mime_type -> true
  | _ -> false

let read_raw_content ?(length = 4096) raw_content =
  let content_stream = Ocsigen_stream.get raw_content in
  Ocsigen_stream.string_of_stream length content_stream

(**** Handlers ****)

let read_handler id_opt () =
  let%lwt db = db in
  match id_opt with
  | None ->
    Ocsipersist.fold_step
      (fun id loc acc -> Lwt.return ((id, loc) :: acc)) db []
    >>= fun locations ->
    let json = Yojson.Safe.to_string (locations_to_yojson locations) in
    send_json ~code:200 json
  | Some id ->
    catch
      (fun () ->
	 Ocsipersist.find db  id >>= fun location ->
	 let json =
	   Yojson.Safe.to_string (location_to_yojson location) in
	 send_json ~code:200 json)
      (function
	| Not_found ->
	  (* [id] hasn't been found, return a "Not found" message *)
	  send_error ~code:404 ("Resource not found: " ^ id)
	| _ -> assert false)

let edit_handler_aux ?(create = false) id_opt (content_type, raw_content_opt) =
  let%lwt db = db in
  if not (check_content_type ~mime_type:json_mime_type content_type) then
    send_error ~code:400 "Content-type is wrong, it must be JSON"
  else
    match id_opt, raw_content_opt with
    | None, _ ->
      send_error ~code:400 "Location identifier is missing"
    | _, None ->
      send_error ~code:400 "Body content is missing"
    | Some id, Some raw_content ->
      read_raw_content raw_content >>= fun location_str ->
      catch (fun () ->
	(if create then
	   Lwt.return_unit
	 else
	   Ocsipersist.find db id >>= fun _ -> Lwt.return_unit)
	>>= fun () ->
	let location =
	  (let open Result in
	   let loc_result = location_of_yojson (Yojson.Safe.from_string location_str) in
	   (function
	       Ok loc-> loc
	     | Error _ -> raise Deriving_Yojson.Failed ) loc_result)  in
	Ocsipersist.add db  id location >>= fun () ->
	send_success ())
	(function
	  | Not_found ->
	    send_error ~code:404 ("Location not found: " ^ id)
	  | Deriving_Yojson.Failed ->
	    send_error ~code:400 "Provided JSON is not valid"
	  | _ -> assert false)

let create_handler id_opt content =
  edit_handler_aux ~create:true id_opt content

let update_handler id_opt content =
  edit_handler_aux ~create:false id_opt content

let delete_handler id_opt _ =
  let%lwt db = db in
  match id_opt with
  | None ->
    send_error ~code:400 "An id must be provided to delete a location"
  | Some id ->
    Ocsipersist.remove db id >>= fun () ->
    send_success ()

(* Register services *)

let () =
  Eliom_registration.Any.register read_service read_handler;
  Eliom_registration.Any.register create_service create_handler;
  Eliom_registration.Any.register update_service update_handler;
  Eliom_registration.Any.register delete_service delete_handler;
  ()
