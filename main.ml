open Lwt.Infix ;;
open Extensions ;;

module Yj = Yojson.Safe ;;

type api_gps_data =
  { lat : float;
    lon : float; } ;;

type api_aps_data = 
  { ssid : string ;
    bssid : string ; } ;;

type api_data = 
    GPS of api_gps_data 
  | APS of api_aps_data list ;;

type api_update = 
  { user : string ;
    data : api_data } ;;

type length_status = LengthOk of int | LengthError ;;
type api = ApiUpdate of Yj.json | ApiQuery of Yj.json | ApiError ;;
type query = QueryAll | QueryLocation of string | QueryUser of string | QueryError of string ;;
type update = Update of api_update | UpdateError of string ;;
exception Input_error of string ;;
exception Fail of string ;; (* Migrate all input errors to this *)

(*Philos: No exceptions! (except those of library... But make wrappers for them! )*)

let return_unit = Lwt.return_unit;;
let return = Lwt.return;;
let max_upload_len = 10*1024 ;; (*10KB*)
let sql_table_name = "user_locations" ;;
let username_max_length = 32 ;;
let sleepy_loop_delay = 220.0 ;;
let stale_db_entry_time = Int64.of_int (60*6) ;; (*20 minutes*)

let location_config = 
  Location.config_of_file "json/locations.json" ;;

let db =
  Sqlite3.db_open ~mode:`NO_CREATE ~mutex:`FULL "db/sampledb.sqlite" ;;

let sql_insert_stmt = 
  Sqlite3.prepare db "INSERT INTO user_locations VALUES (?001, ?002, ?003);" ;;
let sql_reset_db_stmt = 
  Sqlite3.prepare db "DELETE FROM user_locations;" ;;
let sql_query_all_stmt = 
  Sqlite3.prepare db "SELECT * FROM user_locations;" ;;
let sql_query_user_stmt = 
  Sqlite3.prepare db "SELECT * FROM user_locations WHERE (username = ?001);" ;;
let sql_query_location_stmt = 
  Sqlite3.prepare db "SELECT * FROM user_locations WHERE (location = ?001);" ;;

(* Note: DON'T RUN IN OWN THREAD!!! *)
let sqlite_stmt_exec stmt = 
  let open Sqlite3.Rc in
  let to_string = Sqlite3.Rc.to_string in
  let step () = Sqlite3.step stmt in
  let rec walk_sqlite = function
        | BUSY | OK -> walk_sqlite @@ step ()
        | DONE -> ignore @@ Sqlite3.reset stmt
        | state -> begin
            ignore @@ Sqlite3.reset stmt;
            raise @@ Sqlite3.Error (Printf.sprintf "Error in exec_sql_stmt: %s" (to_string state))
        end
  in
  walk_sqlite @@ step ()
;;

let sqlite_bind bindings stmt =
  let open Sqlite3.Rc in
  let bind = Sqlite3.bind in
  let results = List.map (fun (n,d) -> bind stmt n d) bindings in
  let is_okay r = r == OK in
  if List.for_all is_okay results 
    then ignore @@ Sqlite3.prepare_tail stmt
    else raise @@ Sqlite3.Error "Error while binding SQL" ;;

let sqlite_bind_exec bindings stmt = 
  sqlite_bind bindings stmt;
  sqlite_stmt_exec stmt;
  Sqlite3.reset stmt;
;;

let db_exec_log stmt = 
  let code = Sqlite3.exec db stmt in 
  let status = Sqlite3.Rc.to_string code in 
  Lwt_io.printf "db_exec_log: %s (%s)\n" stmt status ;;

let string_of_aps aps =
  String.concat "\n" @@ 
    (List.map 
      (fun a -> Printf.sprintf "    ssid: %s, bssid: %s" a.ssid a.bssid)
      aps) ;;

let string_of_update r = 
  match r.data with
    | GPS g -> 
        Printf.sprintf "GPS Update from '%s':\n   (%f,%f)\n" r.user g.lat g.lon
    | APS a -> begin
        Printf.sprintf "APS Update from '%s':\n%s\n" r.user (string_of_aps a)
    end ;;


let update_of_json (json : Yj.json) : api_update = 
  let to_string = Yj.Util.to_string in
  let to_float = Yj.Util.to_float in
  let err msg = raise @@ Input_error msg in

  let gps_of_json json = 
    { lat = to_float @@ List.assoc "lat" json ;
      lon = to_float @@ List.assoc "lon" json } 
  in

  let ap_of_json = function
    | `Assoc ap 
      -> (try { ssid = to_string @@ List.assoc "ssid" ap ;
                bssid = to_string @@ List.assoc "bssid" ap }
          with Not_found -> err "bssid/ssid not found in ap_of_json")
    | _ -> err "Expected `Assoc in ap_of_json" 
  in

  (* Note: Match will not accept json unless username is first! *)
  match json with
    | `Assoc [("username", `String username); ("aps", `List ap_json_l)]
      -> (try { user = username ;
                data = (APS (List.map ap_of_json ap_json_l)) }
         with Not_found -> err "ssid/bssid not found in ap_of_json")
    | `Assoc [("username", `String username); ("gps", `Assoc json)]
      -> (try { user = username ;
                data = (GPS (gps_of_json json))}
         with Not_found -> err "lat/lon not found in ap_of_json")
    | _ -> err "Invalid structure in Update json" ;;

let print_api_update u = 
  Lwt_io.print @@ string_of_update u ;;

let verify_request_length len =
  if len > max_upload_len 
    then LengthError
    else LengthOk len ;;

let api_of_json = function
  | `Assoc [("Update", json)] -> ApiUpdate json
  | `Assoc [("Query", json)] -> ApiQuery json
  |  _ -> ApiError ;;

let query_of_json = function
  | `String "all" -> QueryAll
  | `Assoc [("location", `String place)] -> QueryLocation place
  | `Assoc [("username", `String name)] -> QueryUser name
  | _ -> QueryError "Invalid query" ;;

let write_update_to_db (u : api_update) = 
  let open Sqlite3.Data in
  let open Location in
  let now = Unix.time_int64 () in
  let location_name = function
    | GPS g -> Location.title_of_latlon (g.lat,g.lon) location_config.gpsl
    | APS a -> Location.title_of_netids 
                (List.map (fun (a : api_aps_data) -> (a.ssid,a.bssid)) a)
                location_config.apl
  in
  let bindings = 
  [ (1, (TEXT u.user)) ;
    (2, (TEXT (location_name u.data))) ;
    (3, (INT now)) ] in
  ignore @@ sqlite_bind_exec bindings sql_insert_stmt;
  return_unit
;;

let sanitize_username s =
  let open String in
  let sane_character = function
    'a'..'z' | 'A'..'Z' | '0'..'9' -> true
    | _ -> false 
  in
  unexplode @@ List.filter sane_character (explode s)
;;

(* Usernames can only contain letters/numbers *)
let sanitize_update_data (d : api_update) = 
  if ((String.length d.user) > username_max_length) 
  then raise @@ Input_error "Username too long"
  else { d with user = sanitize_username d.user } ;;

let json_of_db_query bindings stmt : (Yj.json) = 
  let rec json_of_row row = 
    let to_string = Sqlite3.Data.to_string in
    let username = `String (to_string @@ Array.get row 0) in
    let location = `String (to_string @@ Array.get row 1) in
    let lastupdate = `Intlit (to_string @@ Array.get row 2)  in
    `Assoc [("username", username); ("location", location ); ("lastupdate", lastupdate)]
  in
  let rec walk json_lst = 
    if (Sqlite3.step stmt) = Sqlite3.Rc.DONE
    then json_lst
    else walk ((json_of_row (Sqlite3.row_data stmt)) :: json_lst)
  in
  sqlite_bind bindings stmt;
  let data = `Assoc [("QueryResponse", (`List (walk [])))] in
  ignore @@ Sqlite3.reset stmt;
  data ;;

let query_main io = function
  | QueryUser name -> 
      return @@ json_of_db_query [(1, Sqlite3.Data.TEXT name)] sql_query_user_stmt
      >|= Yojson.Safe.to_string
      >>= Lwt_io.printl
  | QueryLocation place -> 
      return @@ json_of_db_query [(1, Sqlite3.Data.TEXT place)] sql_query_location_stmt
      >|= Yojson.Safe.to_string
      >>= Lwt_io.printl
  | QueryAll -> 
      return @@ json_of_db_query [] sql_query_all_stmt 
      >|= Yojson.Safe.to_string
      >>= Lwt_io.printl
  | _ -> return_unit ;;

let update_main io json = 
  return @@ update_of_json json
  >|= sanitize_update_data
  >>= begin fun d -> 
    write_update_to_db d
    >|= (fun () -> string_of_update d)
    >>= Lwt_io.printl
  end ;;

let api_main io req =
  let open Location in
  match req with
    | ApiUpdate j -> 
        update_main io j
    | ApiQuery j -> 
        return (query_of_json j)
        >>= query_main io
        >>= (fun () -> return_unit)
    | ApiError -> 
        Lwt_io.printl "ApiError" ;;

let server_main io =
  let in_chan,out_chan = io in
  let read_json len = Lwt_io.read ~count:len in_chan in
  let verify_json = Yj.from_string in
    begin Lwt_io.BE.read_int in_chan
      >|= verify_request_length 
      >>= begin function 
         | LengthError -> 
             Lwt_io.printf "Length must be < %d\n" max_upload_len
         | LengthOk len -> 
             begin
               read_json len
               >|= verify_json
               >|= api_of_json 
               >>= api_main io
         end
      end
  end ;;

let background_tasks () = 
  let remove_older_than = 
    Int64.sub (Unix.time_int64 ()) stale_db_entry_time in
  let sql = Printf.sprintf
    "DELETE FROM user_locations WHERE (last_update_time <= %Ld);" 
    remove_older_than 
  in
  db_exec_log sql ;;

let rec sleepy_loop delay = 
  Lwt_unix.sleep delay
  >>= (fun () -> background_tasks ())
  >>= (fun () -> sleepy_loop delay) ;;

let start_server io = 
  let sprintf = Printf.sprintf in

  let stop_server msg = 
    let in_chan,out_chan = io in
    Lwt_io.printl msg 
    >>= (fun () -> Lwt_io.close in_chan) 
  in

  let start_server () = 
    Lwt.catch 
      (fun () -> 
        server_main io
        >>= (fun () -> stop_server "Success!"))
      (function                    (* Where fatal errors are caught *)
        | Input_error s -> 
            stop_server ("Input_error: " ^ s)
        | Yojson.Json_error msg -> 
            stop_server (sprintf "Json_error: %s" msg)
        | Yj.Util.Type_error (msg,json) ->
            stop_server (sprintf "Type_error: %s (json = %s)" msg (Yj.to_string json))
        | e -> raise e)
  in
  Lwt.async start_server
;;

let main () =
  let port,buffsz = 9993,(1024*1024*2) in
  let listen_addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
  let clear_database () = sqlite_stmt_exec sql_reset_db_stmt in

  Lwt_io.print @@ Location.string_of_location_conf location_config
  >>= (fun () -> Lwt_io.printf "Started server on port %d\n" port)
  >|= clear_database
  >>= (fun () ->
    Lwt.async 
    begin fun () -> 
      return @@ 
        Lwt_io.establish_server 
        ~backlog:20
        ~buffer_size:buffsz
        listen_addr
        start_server
    end ;
    sleepy_loop sleepy_loop_delay) ;;

Printexc.record_backtrace true ;;

Lwt_main.run @@ main () ;;
