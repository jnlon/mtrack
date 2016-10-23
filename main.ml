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
  { id : string ;
    user : string ;
    data : update_data } ;;

type create_data =
  { user : string } ;;

type api = Update of Yj.json | Query of Yj.json | Create of Yj.json ;;
type query = QueryAll | QueryLocation of string | QueryUser of string ;;
exception Api_err of string ;;

(* Shorthands *)
let return_unit = Lwt.return_unit ;;
let return = Lwt.return ;;
let sprintf = Printf.sprintf ;;
let printfl = Lwt_io.printf ;;
let printl = Lwt_io.printl ;;
let async = Lwt.async ;;

(* Constants / Configuration paramaters *)
module Const = struct
  let max_upload_len = 10*1024 ;; (*10KB*)
  let max_username_length = 32 ;;
  let db_max_query_rows = 50 ;;
  let db_file_path = "db/sampledbV2.sqlite" ;;
  let sql_table_name = "user_locations" ;;
  let sleepy_loop_delay = 220.0 ;;
  let id_char_length = 8 ;;
  let stale_db_entry_time = Int64.of_int (60*10) ;; (* in seconds *) 
  let block_uncreated_ids = false ;;    (* clients must have a user id to make requests *)
  let location_config = 
    Location.config_of_file "json/locations.json" ;;
  let backlog = 50 ;;
  let buffer_size = 20480 ;;
  let port = 9993 ;;
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
    Sqlite3.db_open ~mode:`NO_CREATE ~mutex:`NO Const.db_file_path ;;
  let sql_insert_stmt = Sqlite3.prepare db 
    "INSERT INTO user_locations VALUES (?001, ?002, ?003, ?004);" ;;
  let sql_query_id_stmt = Sqlite3.prepare db
    "SELECT 1 FROM user_locations WHERE userid=?001"
  let sql_reset_db_stmt = Sqlite3.prepare db 
    "DELETE FROM user_locations;" ;;
  let sql_query_all_stmt = Sqlite3.prepare db 
    "SELECT * FROM user_locations ORDER BY last_update_time DESC;" ;;
  let sql_query_user_stmt = Sqlite3.prepare db 
    "SELECT * FROM user_locations WHERE (username = ?001);" ;;
  let sql_query_location_stmt = Sqlite3.prepare db 
    "SELECT * FROM user_locations WHERE (location = ?001) ORDER BY last_update_time DESC;" ;;
  let sql_delete_older_than_stmt time = Sqlite3.prepare db 
    (sprintf "DELETE FROM user_locations WHERE (last_update_time <= %Ld);" time)

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

  let sqlite_bind_exec bindings stmt = 
    reset_stmt stmt;
    sqlite_bind bindings stmt;
    sqlite_stmt_exec stmt ;;

  type db_entry = 
    { userid : string ;
      username : string ;
      location : string ;
      last_update_time : int64 } ;;

  type query_db_entry = 
    { username : string ;
      location : string ;
      last_update_time : int64 } ;;

  let query_users bindings stmt : (query_db_entry list) = 
    let db_entry_of_row row = 
      { username = string_of_db_text @@ Array.get row 1 ;
        location = string_of_db_text @@ Array.get row 2 ;
        last_update_time = int64_of_db_int @@ Array.get row 3 }
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

  let user_id_exists id =
    reset_stmt sql_query_id_stmt;
    sqlite_bind [(1, Sqlite3.Data.TEXT id)] sql_query_id_stmt;
    let results = 
        (List.accumulate 
          (fun () -> Sqlite3.step sql_query_id_stmt) 
          (fun r -> List.mem r [Sqlite3.Rc.DONE])) 
    in
    let found_an_id = 
      List.mem Sqlite3.Rc.ROW results
    in
    (*String.concat "\n" (List.map Sqlite3.Rc.to_string results)*)
    found_an_id 
  ;;

  let query_by_name uname =
    query_users [(1, Sqlite3.Data.TEXT uname)] sql_query_user_stmt ;;

  let query_by_location place =
    query_users [(1, Sqlite3.Data.TEXT place)] sql_query_location_stmt ;;

  let query_all () =
    query_users [] sql_query_all_stmt ;;

  let delete_older_than time = 
    sqlite_bind_exec [] (sql_delete_older_than_stmt time) ;;

  let update_user id user place time =
    let bindings = 
    [ (1, (Sqlite3.Data.TEXT id)) ;
      (2, (Sqlite3.Data.TEXT user)) ;
      (3, (Sqlite3.Data.TEXT place)) ;
      (4, (Sqlite3.Data.INT time))] in
    sqlite_bind_exec bindings sql_insert_stmt ;;

end

let string_of_aps aps =
  let string_of_ap a = sprintf "  ssid: %s, bssid: %s" a.ssid a.bssid in
  String.concat "\n" @@ 
    (List.map string_of_ap aps) ;;

let string_of_update r = 
  match r.data with
    | GPS g -> 
        sprintf "GPS Update from '%s':\n  (%f,%f)" r.id g.lat g.lon
    | APS a ->
        sprintf "APS Update from '%s':\n%s" r.id (string_of_aps a) ;;

let update_of_json (json : Yj.json) : api_update = 
  let to_string = Yj.Util.to_string in
  let to_float = Yj.Util.to_float in
  let gps_of_json json = 
    try { lat = to_float @@ List.assoc "lat" json ;
          lon = to_float @@ List.assoc "lon" json } 
    with Not_found -> raise @@ Api_err "lat/lon not found in GPS data"
  in
  let ap_of_json = function
    | `Assoc ap 
      -> (try { ssid = to_string @@ List.assoc "ssid" ap ;
                bssid = to_string @@ List.assoc "bssid" ap }
          with Not_found -> raise @@ Api_err  "bssid/ssid not found in AP data")
    | _ -> raise @@ Api_err "Expected `Assoc in ap_of_json" 
  in
  match json with
    | `Assoc [("id", `String id); ("username", `String username); data_json]
    | `Assoc [("username", `String username); ("id", `String id); data_json] 
      -> (let data = 
            match data_json with
              | ("aps", `List ap_json_l) -> (APS (List.map ap_of_json ap_json_l))
              | ("gps", `Assoc json) -> (GPS (gps_of_json json))
              | _ -> raise @@ Api_err "Invalid data structure in update json" in
          {id = id ; user = username ; data = data})
    | _ -> raise @@ Api_err "Invalid data structure in update json" ;;

let verify_request_length len =
  if len > Const.max_upload_len 
    then raise @@ Api_err "Upload length exceeds maximum"
    else len ;;

let api_of_json = function
  | `Assoc [("Update", json)] -> Update json
  | `Assoc [("Query", json)] -> Query json
  | `Assoc [("Create", json)] -> Create json
  | `Assoc [(c, _)] -> raise @@ Api_err (sprintf "Unknown API command '%s'" c)
  |  _ -> raise @@ Api_err "Invalid API format" ;;

let query_of_json = function
  | `String "all" -> QueryAll
  | `Assoc [("location", `String place)] -> QueryLocation place
  | `Assoc [("username", `String name)] -> QueryUser name
  | _ -> raise @@ Api_err "Invalid query format" ;;

let write_update_to_db (u : api_update) = 
  let open Location in
  let time_now = Unix.time_int64 () in
  let location_name = 
    match u.data with
      | GPS g -> Location.title_of_latlon (g.lat,g.lon) Const.location_config.gpsl
      | APS a -> Location.title_of_netids 
                  (List.map (fun (a : ap_update_data) -> (a.ssid,a.bssid)) a)
                  Const.location_config.apl in

  Sql.update_user u.id u.user location_name time_now;
  return_unit ;;

(* Usernames can only contain letters/numbers *)
let verify_update_data (d : api_update) = 
  let uname_chars_ok s =
    let sane_character = function
      | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
      | _ -> false in
    List.for_all sane_character (String.explode s) in
  let uname_length_ok = 
    (String.length d.user) <= Const.max_username_length in
  if (uname_length_ok && (uname_chars_ok d.user))
  then d
  else raise @@ Api_err ("Username invalid: " ^ d.user ) ;;

let json_of_db_query (ulq_l : Sql.query_db_entry list) =
  let open Sql in
  let json_of_ulq ulq = 
    `Assoc [("username", `String ulq.username);
            ("location", `String ulq.location);
            ("lastupdate", `Intlit (Int64.to_string ulq.last_update_time))]
  in
  `Assoc [("QueryResponse", (`List (List.map json_of_ulq ulq_l)))] ;;

let create_of_json = function
  | `Assoc [("username", `String user)] -> {user=user} 
  | _ -> raise @@ Api_err "Invalid Create json structure" ;;

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

let update_main io json = 
  return @@ verify_update_data json
  >>= begin fun d -> 
    write_update_to_db d
    >|= (fun () -> string_of_update d)
    >>= Lwt_io.printl
  end ;;

let rec create_user_id () = 
  let buf = Bytes.create Const.id_char_length in
  let rec gen_id i = 
    let ch = char_of_int (Random.int 127) in
    match ch with
      | _ when i = (Const.id_char_length) 
          -> Bytes.to_string buf
      | 'a'..'z' | 'A'..'Z' | '0'..'9' 
          -> (Bytes.set buf i ch; gen_id (i + 1))
      | _ -> gen_id i
  in
  let id = (gen_id 0) 
  in
  if Sql.user_id_exists id
    then create_user_id ()
    else id ;;

let api_main io = function
  | Update j -> 
      return @@ update_of_json j
      >>= update_main io
  | Query j -> 
      return @@ query_of_json j
      >>= query_main io 
  | Create j -> 
      return @@ create_of_json j
      >>= begin fun cdata -> 
        return @@ create_user_id ()
        >>= Lwt_io.printl
      end ;;

let server_main io =
  let in_chan,out_chan = io in
  let read_json ch len = Lwt_io.read ~count:len ch in
  let verify_json = Yj.from_string in
  Lwt_io.BE.read_int in_chan
  >|= verify_request_length 
  >>= read_json in_chan
  >|= verify_json
  >|= api_of_json 
  >>= api_main io ;;

let background_tasks () = 
  let too_old = 
    Int64.sub (Unix.time_int64 ()) Const.stale_db_entry_time in
  Lwt_io.printf "Removing older than %Ld\n" too_old
  >|= (fun () -> Sql.delete_older_than too_old) ;;

let rec sleepy_loop delay = 
  Lwt_unix.sleep delay
  >>= (fun () -> background_tasks ())
  >>= (fun () -> sleepy_loop delay) ;;

let start_server io = 
  let log_stop_server msg = 
    let in_chan,out_chan = io in
    Lwt_io.printl msg
    >>= (fun () -> Lwt_io.close in_chan) 
  in

  let start_server () = 
    Lwt.catch 
      (fun () -> 
        server_main io
        >>= (fun () -> Lwt_io.close (fst io)))
      (function                    (* Where fatal errors are caught *)
        | Api_err s -> 
            log_stop_server ("API Error: " ^ s)
        | Sqlite3.Error msg -> 
            log_stop_server (sprintf "Sqlite3.Error: %s" msg)
        | Yojson.Json_error msg -> 
            log_stop_server (sprintf "Json_error: %s" msg)
        | Yj.Util.Type_error (msg,json) ->
            log_stop_server (sprintf "Type_error: %s (json = %s)" msg (Yj.to_string json))
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

Random.self_init ();
Printexc.record_backtrace true ;;
Lwt_main.run @@ main () ;;
