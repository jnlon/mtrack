open Lwt.Infix ;;
open Extensions ;;

let return_unit = Lwt.return_unit;;
let return = Lwt.return;;

type json_t = Yojson.Safe.json ;;
type length_status = LengthOk of int | LengthError ;;
type json_status = JsonOk of json_t | JsonError of string ;;
type api = ApiUpdate of json_t | ApiQuery of json_t | ApiError ;;
type query = 
  QueryAll | QueryLocation of string | QueryUser of string | QueryError ;;

type api_update_t = 
  { user : string ;
    lat : float ;
    lon : float } ;;

exception Update_error of string ;;

let spf = Printf.sprintf ;;
let max_upload_len = 10*1024 ;; (*10KB*)
let sql_table_name = "user_locations" ;;
let username_max_length = 32 ;;
let sleepy_loop_delay = 10.0 ;;
let stale_db_entry_time = Int64.of_int (60*6) ;; (*20 minutes*)

let db =
  Sqlite3.db_open ~mode:`NO_CREATE ~mutex:`FULL "db/sampledb.sqlite" ;;

let sql_insert_string = "INSERT INTO user_locations VALUES (?001, ?002, ?003);" ;;
let sql_insert_stmt = Sqlite3.prepare db sql_insert_string ;;

let sql_reset_db_stmt = Sqlite3.prepare db "DELETE FROM user_locations;" ;;

let sql_query_all_stmt = Sqlite3.prepare db "SELECT * FROM user_locations;" ;;
let sql_query_user_stmt = Sqlite3.prepare db "SELECT * FROM user_locations WHERE (username = ?001);" ;;
let sql_query_location_stmt = Sqlite3.prepare db "SELECT * FROM user_locations WHERE (location = ?001);" ;;

let locate_config = Location.config_of_path "json/locations.json" ;;

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
            raise @@ Sqlite3.Error (spf "Error in exec_sql_stmt: %s" (to_string state))
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

let string_of_update u =
  Printf.sprintf "'%s' at (%f,%f)" u.user u.lat u.lon ;;

let fini msg io = 
 let inch,_ = io in
 Lwt_io.printl msg 
 >>= (fun () -> Lwt_io.close inch) ;;

let update_of_json json =
  let open Yojson.Safe.Util in
  try
    match json with 
      | `Assoc json_l ->
            { user = to_string @@ List.assoc "username" json_l ;
              lat = to_float @@ List.assoc "lat" json_l ;
              lon = to_float @@ List.assoc "lon" json_l }
      | _ -> raise (Update_error "Expecting an object associated with 'Update'\n")
  with 
    | Not_found -> raise @@ Update_error "Missing fields in 'Update' object"
    | Type_error (errmsg,json) -> raise @@ Update_error errmsg
    | Update_error (errmsg) -> raise @@ Update_error errmsg ;;

let print_api_update_data d = 
  Lwt_io.printf "Update: user%s lat=%f lon=%f\n" d.user d.lat d.lon ;;

let verify_request_length len =
  Lwt_io.printf "Request length is %d bytes\n" len
  >|= begin 
    fun () ->
      if len > max_upload_len 
        then LengthError
        else LengthOk len end ;;

let verify_json str = 
  let open Yojson in
  return begin
    try (JsonOk (Safe.from_string str))
    with Yojson.Json_error msg -> JsonError msg
  end ;;

let api_of_json = function
  | `Assoc [("Update", json)] -> ApiUpdate json
  | `Assoc [("Query", json)] -> ApiQuery json
  |  _ -> ApiError ;;

let query_of_json = function
  | `String "all" -> QueryAll
  | `Assoc [("location", `String place)] -> QueryLocation place
  | `Assoc [("username", `String name)] -> QueryUser name
  | _ -> QueryError
;;

let write_update_to_db u = 
  let open Sqlite3.Data in
  let now = Unix.time_int64 () in
  let location_name = Location.title_of_latlon (u.lat,u.lon) in
  let bindings = 
  [ (1, (TEXT u.user)) ;
    (2, (TEXT location_name)) ;
    (3, (INT now)) ] in
  sqlite_bind_exec bindings sql_insert_stmt;
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
let sanitize_update_data (d : api_update_t) = 
  if ((String.length d.user) > username_max_length) 
  then raise @@ Update_error "Username too long"
  else { d with user = sanitize_username d.user } ;;

let json_of_db_query bindings stmt : (Yojson.Safe.json) = 
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
  Sqlite3.reset stmt;
  data
;;


(* Revelation: step increments what row we are on in the current query, and the
 * column, row_data functions allow us to retrieve data from the current row...
 * *)

let query_main io query =
  match query with
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
    | _ -> return_unit
;;

let api_main io req =
  let open Location in
  let fini msg = fini msg io in
  let update_of_json = Lwt.wrap1 (fun j -> update_of_json j) in
  match req with
    | ApiUpdate j -> 
        Lwt.catch 
        begin fun () ->
          update_of_json j
          >|= sanitize_update_data
          >>= begin fun d -> 
            write_update_to_db d
            >|= (fun () -> string_of_update d)
            >>= fini end end
        (function 
          | Update_error s -> fini (spf "Update_error: %s" s)
          | _ -> fini "unknown error???")
    | ApiQuery j -> 
        return (query_of_json j)
        >>= (fun d -> query_main io d)
        (*>>= do something with json struct returned from the query... *)
        >>= (fun () -> fini "Query Finished")
    | ApiError -> fini "Unknown API request" ;;

let server_main io =
  let open Lwt_io in
  let in_chan,out_chan = io in
  let fini msg = fini msg io in
    begin BE.read_int in_chan
      >>= verify_request_length 
      >>= begin function 
         | LengthError -> fini "Length Error"
         | LengthOk len -> begin
            read ~count:len in_chan
              >>= verify_json
              >>= begin function 
                | JsonError msg -> fini ("Json error: " ^ msg)
                | JsonOk json -> api_main io @@ api_of_json json
              end
          end
      end
  end ;;

let background_tasks () = 
  let remove_older_than = 
    Int64.sub (Unix.time_int64 ()) stale_db_entry_time in
  let sql = spf "DELETE FROM user_locations WHERE (last_update_time <= %Ld);" remove_older_than 
  in
  db_exec_log sql ;;

let rec sleepy_loop delay = 
  Lwt_unix.sleep delay
  >>= (fun () -> background_tasks ())
  >>= (fun () -> sleepy_loop delay) ;;

let main () =
  let port,bsz = 9993,(1024*1024*2) in
  let listen_addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
  let start_server = (fun io -> Lwt.async (fun () -> server_main io)) in

  Lwt_io.print @@ Location.string_of_locations ()
  >>= (fun () -> Lwt_io.printf "Started server on port %d\n" port)
  >|= (fun () -> sqlite_stmt_exec sql_reset_db_stmt)
  >>= (fun () ->
    Lwt.async 
    begin fun () -> 
      return @@ Lwt_io.establish_server ~backlog:20 ~buffer_size:bsz listen_addr start_server;
    end ;
    sleepy_loop sleepy_loop_delay) ;;

(*Printexc.record_backtrace true ;;*)

Lwt_main.run @@ main () ;;
