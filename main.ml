open Lwt.Infix ;;
open Extensions ;;

module Yj = Yojson.Safe ;;

type gps_update_data =
  { lat : float;
    lon : float; } ;;

type ap_update_data = 
  { ssid : string ;
    bssid : string ; } ;;

type update_data = 
    GPS of gps_update_data 
  | APS of ap_update_data list ;;

type api_update = 
  { user : string ;
    data : update_data } ;;

type length_status = LengthOk of int | LengthError ;;
type api = ApiUpdate of Yj.json | ApiQuery of Yj.json | ApiError of string ;;
type query = QueryAll | QueryLocation of string | QueryUser of string | QueryError of string ;;
type update = Update of api_update | UpdateError of string ;;
exception Api_err of string ;;

(* Shorthands *)
let return_unit = Lwt.return_unit ;;
let return = Lwt.return ;;
let sprintf = Printf.sprintf ;;

(* Constants / Configuration paramaters *)
module Const = struct
  let max_upload_len = 10*1024 ;; (*10KB*)
  let sql_table_name = "user_locations" ;;
  let username_max_length = 32 ;;
  let sleepy_loop_delay = 220.0 ;;
  let stale_db_entry_time = Int64.of_int (60*10) ;; (* in seconds *) 
  let db_file_path = "db/sampledb.sqlite" ;;
  let location_config = 
    Location.config_of_file "json/locations.json" ;;
  let backlog = 25 ;; (* Connections *)
  let port = 9993 ;;
  let buffer_size = 20480 ;;
  let db_max_query_rows = 50 ;;
end

(* SQL definitions *)

module Sql = struct

  exception Bad_conv of string
  let int64_of_db_int = function Sqlite3.Data.INT i64 -> i64 | _ -> raise @@ Bad_conv "int64" ;;
  let float_of_db_float = function Sqlite3.Data.FLOAT f -> f | _ -> raise @@ Bad_conv "float" ;;
  let string_of_db_text = function Sqlite3.Data.TEXT txt -> txt | _ -> raise @@ Bad_conv "text" ;;
  let string_of_db_blob = function Sqlite3.Data.BLOB blb -> blb | _ -> raise @@ Bad_conv "blob" ;;

  (* TODO: Define a separate module, and make these private through .mli *)
  let db =
    Sqlite3.db_open ~mode:`NO_CREATE ~mutex:`FULL Const.db_file_path ;;
  let sql_insert_stmt = Sqlite3.prepare db 
    "INSERT INTO user_locations VALUES (?001, ?002, ?003);" ;;
  let sql_reset_db_stmt = Sqlite3.prepare db 
    "DELETE FROM user_locations;" ;;
  let sql_query_all_stmt = Sqlite3.prepare db 
    "SELECT * FROM user_locations ORDER BY last_update_time DESC;" ;;
  let sql_query_user_stmt = Sqlite3.prepare db 
    "SELECT * FROM user_locations WHERE (username = ?001);" ;;
  let sql_query_location_stmt = Sqlite3.prepare db 
    "SELECT * FROM user_locations WHERE (location = ?001) ORDER BY last_update_time DESC;" ;;

  let reset_stmt stmt =
    let open Sqlite3.Rc in
    let cb = Sqlite3.clear_bindings stmt in
    let rst = Sqlite3.reset stmt in
    match cb,rst with
      | (OK,OK) -> ()
      | _ -> raise @@ Sqlite3.Error "Could not reset stmt!" ;;

  let sqlite_stmt_exec stmt = 
    let open Sqlite3.Rc in
    let step () = Sqlite3.step stmt in
    let rec walk_sqlite = function
          | BUSY | OK -> walk_sqlite @@ step ()
          | DONE -> ignore @@ Sqlite3.reset stmt
          | state -> 
              raise @@ Sqlite3.Error (sprintf "Error in exec_sql_stmt: %s" (to_string state))
    in
    walk_sqlite @@ step () ;;

  let sqlite_bind bindings stmt =
    let results = List.map (fun (n,d) -> Sqlite3.bind stmt n d) bindings in
    let is_okay r = (r == Sqlite3.Rc.OK) in
    if List.for_all is_okay results 
      then ()
      else raise @@ Sqlite3.Error ("Error binding variables to SQL statements") ;;

  let reset_db () = 
    reset_stmt sql_reset_db_stmt;
    sqlite_stmt_exec sql_reset_db_stmt ;;

  type db_entry = 
    { username : string ;
      location : string ;
      last_update_time : int64 } ;;

  let query_users bindings stmt : (db_entry list) = 
    let db_entry_of_row row = 
      { username = string_of_db_text @@ Array.get row 0 ;
        location = string_of_db_text @@ Array.get row 1 ;
        last_update_time = int64_of_db_int @@ Array.get row 2 }
    in

    let rec walk_rows i = 
      if i >= Const.db_max_query_rows then [] 
      else begin
        let open Sqlite3.Rc in
        match Sqlite3.step stmt with
            DONE -> []
          | ROW | OK -> begin
              let data = Sqlite3.row_data stmt in
              if Array.length data > 0 
              then (db_entry_of_row data) :: walk_rows (i + 1)
              else walk_rows i
          end
          | r -> walk_rows i
      end
    in

    reset_stmt stmt;
    sqlite_bind bindings stmt;
    walk_rows 0
  ;; 

  let query_by_name uname =
    query_users [(1, Sqlite3.Data.TEXT uname)] sql_query_user_stmt ;;

  let query_by_location place =
    query_users [(1, Sqlite3.Data.TEXT place)] sql_query_location_stmt ;;

  let query_all () =
    query_users [] sql_query_all_stmt ;;

  let sqlite_bind_exec bindings stmt = 
    reset_stmt stmt;
    sqlite_bind bindings stmt;
    sqlite_stmt_exec stmt ;;

  let update_user user place time =
    let bindings = 
    [ (1, (Sqlite3.Data.TEXT user)) ;
      (2, (Sqlite3.Data.TEXT place)) ;
      (3, (Sqlite3.Data.INT time))] in
    sqlite_bind_exec bindings sql_insert_stmt ;;

  let db_exec_log stmt = 
    let code = Sqlite3.exec db stmt in 
    let status = Sqlite3.Rc.to_string code in 
    Lwt_io.printf "db_exec_log: %s (%s)\n" stmt status ;;

end

let string_of_aps aps =
  let string_of_ap a = sprintf "  ssid: %s, bssid: %s" a.ssid a.bssid in
  String.concat "\n" @@ 
    (List.map string_of_ap aps) ;;

let string_of_update r = 
  match r.data with
    | GPS g -> 
        sprintf "GPS Update from '%s':\n  (%f,%f)\n" r.user g.lat g.lon
    | APS a ->
        sprintf "APS Update from '%s':\n%s\n" r.user (string_of_aps a) ;;

let update_of_json (json : Yj.json) : api_update = 
  let to_string = Yj.Util.to_string in
  let to_float = Yj.Util.to_float in
  let err msg = raise @@ Api_err msg in

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
  if len > Const.max_upload_len 
    then LengthError
    else LengthOk len ;;

let api_of_json = function
  | `Assoc [("Update", json)] -> ApiUpdate json
  | `Assoc [("Query", json)] -> ApiQuery json
  | `Assoc [(c, _)] -> ApiError ("Unknown command: " ^ c)
  |  _ -> ApiError "Invalid json format" ;;

let query_of_json = function
  | `String "all" -> QueryAll
  | `Assoc [("location", `String place)] -> QueryLocation place
  | `Assoc [("username", `String name)] -> QueryUser name
  | _ -> QueryError "Invalid query" ;;

let write_update_to_db (u : api_update) = 
  let open Sqlite3.Data in
  let open Location in
  let time_now = Unix.time_int64 () in
  let location_name = 
    match u.data with
      | GPS g -> Location.title_of_latlon (g.lat,g.lon) Const.location_config.gpsl
      | APS a -> Location.title_of_netids 
                  (List.map (fun (a : ap_update_data) -> (a.ssid,a.bssid)) a)
                  Const.location_config.apl in

  Sql.update_user u.user location_name time_now;
  return_unit ;;

let sanitize_username s =
  let open String in
  let sane_character = function
    'a'..'z' | 'A'..'Z' | '0'..'9' -> true
    | _ -> false 
  in
  unexplode @@ List.filter sane_character (explode s) ;;

(* Usernames can only contain letters/numbers *)
let sanitize_update_data (d : api_update) = 
  if ((String.length d.user) > Const.username_max_length) 
  then raise @@ Api_err "Username too long"
  else { d with user = sanitize_username d.user } ;; (* TODO! Just error out when uname fails! *)


let json_of_db_query (ulq_l : Sql.db_entry list) =
  let open Sql in
  let json_of_ulq ulq = 
    `Assoc [("username", `String ulq.username);
            ("location", `String ulq.location);
            ("lastupdate", `Intlit (Int64.to_string ulq.last_update_time))]
  in
  `Assoc [("QueryResponse", (`List (List.map json_of_ulq ulq_l)))] ;;

let query_main io = function
  | QueryUser name -> 
      return @@ Sql.query_by_name name
      >|= json_of_db_query
      >|= Yojson.Safe.to_string
      >>= Lwt_io.printl
  | QueryLocation place -> 
      return @@ Sql.query_by_location place
      >|= json_of_db_query
      >|= Yojson.Safe.to_string
      >>= Lwt_io.printl
  | QueryAll -> 
      return @@ Sql.query_all ()
      >|= json_of_db_query
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
    | ApiError msg -> 
        Lwt_io.printf "ApiError: %s" msg ;;

let server_main io =
  let in_chan,out_chan = io in
  let read_json ch len = Lwt_io.read ~count:len ch in
  let verify_json = Yj.from_string in
    begin Lwt_io.BE.read_int in_chan
      >|= verify_request_length 
      >>= begin function 
         | LengthError -> 
             Lwt_io.printf "Length must be < %d\n" Const.max_upload_len
         | LengthOk len -> 
             begin
               read_json in_chan len
               >|= verify_json
               >|= api_of_json 
               >>= api_main io
         end
      end
  end ;;

let background_tasks () = 
  let remove_older_than = 
    Int64.sub (Unix.time_int64 ()) Const.stale_db_entry_time in
  let sql = sprintf
    "DELETE FROM user_locations WHERE (last_update_time <= %Ld);" 
    remove_older_than 
  in
  Sql.db_exec_log sql ;;

let rec sleepy_loop delay = 
  Lwt_unix.sleep delay
  >>= (fun () -> background_tasks ())
  >>= (fun () -> sleepy_loop delay) ;;

let start_server io = 
  let stop_server msg = 
    let in_chan,out_chan = io in
    return_unit
    (*Lwt_io.printl msg*)
    >>= (fun () -> Lwt_io.close in_chan) 
  in

  let start_server () = 
    Lwt.catch 
      (fun () -> 
        server_main io
        >>= (fun () -> stop_server "Success!"))
      (function                    (* Where fatal errors are caught *)
        | Api_err s -> 
            stop_server ("API Error: " ^ s)
        | Yojson.Json_error msg -> 
            stop_server (sprintf "Json_error: %s" msg)
        | Yj.Util.Type_error (msg,json) ->
            stop_server (sprintf "Type_error: %s (json = %s)" msg (Yj.to_string json))
        | e -> raise e)
  in
  Lwt.async start_server
;;

let main () =
  (*let port,buffsz = 9993,(1024*1024*2) in*)
  let listen_addr = Unix.ADDR_INET (Unix.inet_addr_any, Const.port) in

  Lwt_io.print @@ Location.string_of_location_conf Const.location_config
  >>= (fun () -> Lwt_io.printf "Started server on port %d\n" Const.port)
  >|= Sql.reset_db
  >>= (fun () ->
    Lwt.async 
    begin fun () -> 
      return @@ 
        Lwt_io.establish_server 
        ~backlog:Const.backlog
        ~buffer_size:Const.buffer_size
        listen_addr
        start_server
    end ;
    sleepy_loop Const.sleepy_loop_delay) ;;

Printexc.record_backtrace true ;;

Lwt_main.run @@ main () ;;
