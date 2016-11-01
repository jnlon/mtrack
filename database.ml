module SD = Sqlite3.Data ;;
module S = Sqlite3 ;;
open Extensions ;;

exception SqliteConstraint ;;
exception Bad_conv of string

let int64_of_db_int = 
  function SD.INT i64 -> i64 | SD.NULL -> 0L | _ -> raise @@ Bad_conv "int64" ;;
let string_of_db_text = 
  function SD.TEXT txt -> txt | SD.NULL -> "" | _ -> raise @@ Bad_conv "text" ;;

let sprintf = Printf.sprintf ;;

let db =
  Sqlite3.db_open ~mode:`NO_CREATE ~mutex:`FULL Const.db_file_path ;;
let sql_update_user_stmt = Sqlite3.prepare db 
  "UPDATE user_locations SET location=?002, last_update_time=?003 WHERE userid=?001" ;;
let sql_create_user_stmt = Sqlite3.prepare db 
  "INSERT INTO user_locations (userid,username,location,last_update_time,creation_time)
   VALUES (?001, ?002, '', ?003, ?004);" ;;
let sql_query_username_stmt = Sqlite3.prepare db
  "SELECT * FROM user_locations WHERE username=?001"
let sql_query_id_stmt = Sqlite3.prepare db
  "SELECT * FROM user_locations WHERE userid=?001"
let sql_reset_db_stmt = Sqlite3.prepare db 
  "DELETE FROM user_locations;" ;;
let sql_query_all_stmt = Sqlite3.prepare db 
  "SELECT * FROM user_locations ORDER BY last_update_time DESC;" ;;
let sql_query_users_stmt n = 
  let identifiers = String.concat ", " (List.repeat "?" n) in
  let stmtsrc = Printf.sprintf 
    "SELECT * FROM user_locations WHERE username IN (%s);" identifiers in
  Sqlite3.prepare db stmtsrc ;;
let sql_query_location_stmt = Sqlite3.prepare db 
  "SELECT * FROM user_locations WHERE (location = ?001)
   ORDER BY last_update_time DESC;" ;;
let sql_delete_older_than_stmt time = Sqlite3.prepare db 
  (sprintf "DELETE FROM user_locations WHERE (last_update_time <= %Ld);" time)

let rec reset_stmt stmt =
  let open Sqlite3.Rc in
  let cb = Sqlite3.clear_bindings stmt in
  let rst = Sqlite3.reset stmt in
  match cb,rst with
    | (OK,OK) -> ()
    | _ -> reset_stmt stmt ;;

let sqlite_stmt_exec stmt = 
  let open Sqlite3.Rc in
  let step () = Sqlite3.step stmt in
  let rec walk_sqlite = function
        | BUSY | OK -> walk_sqlite @@ step ()
        | DONE -> ()
        | CONSTRAINT -> raise SqliteConstraint
        | state -> 
            raise @@ Sqlite3.Error (sprintf "Error in exec_sql_stmt: %s" (to_string state))
  in
  walk_sqlite @@ step () ;;

let sqlite_bind bindings stmt =
  let results = List.map (fun (n,d) -> Sqlite3.bind stmt n d) bindings in
  let is_okay r = (r == Sqlite3.Rc.OK) in
  if List.for_all is_okay results 
    then ()
    else raise @@ Sqlite3.Error "Error binding variables to SQL statements" ;;

let reset_db () = 
  reset_stmt sql_reset_db_stmt;
  sqlite_stmt_exec sql_reset_db_stmt ;;

let sqlite_bind_exec bindings stmt = 
  reset_stmt stmt;
  sqlite_bind bindings stmt;
  sqlite_stmt_exec stmt ;;

type query_db_entry = 
  { userid : string ;
    username : string ;
    location : string ;
    last_update_time : int64 ;
    creation_time : int64 } ;;

let query_db bindings stmt : (query_db_entry list) = 
  let db_entry_of_row row = 
    { userid = string_of_db_text @@ Array.get row 0 ;
      username = string_of_db_text @@ Array.get row 1 ;
      location = string_of_db_text @@ Array.get row 2 ;
      last_update_time = int64_of_db_int @@ Array.get row 3 ;
      creation_time = int64_of_db_int @@ Array.get row 4 ; }
  in
  let rec walk_rows i = 
    if i >= Const.db_max_query_rows then [] 
    else begin
      let open Sqlite3.Rc in
      match Sqlite3.step stmt with
          DONE -> []
        | ROW -> begin
            let data = Sqlite3.row_data stmt in
            (db_entry_of_row data) :: walk_rows (i + 1)
        end
        | r -> walk_rows i
    end
  in
  reset_stmt stmt;
  sqlite_bind bindings stmt;
  walk_rows 0 ;; 

let user_id_exists id =
  let query = query_db [1, (SD.TEXT id)] sql_query_id_stmt in
  (List.length query) > 0 ;;

let user_name_exists name =
  let query = query_db [1, (SD.TEXT name)] sql_query_username_stmt in
  (List.length query) > 0 ;;

let query_by_names names =
  let stmt = sql_query_users_stmt (List.length names) in
  let bindings = List.mapi (fun i name -> ((i+1), SD.TEXT name)) names in
  query_db bindings stmt ;;

let query_by_location place =
  query_db [(1, SD.TEXT place)] sql_query_location_stmt ;;

let query_all () =
  query_db [] sql_query_all_stmt ;;

let delete_older_than time = 
  sqlite_bind_exec [] (sql_delete_older_than_stmt time) ;;

let update_user id place time =
  let bindings = 
  [ (1, (SD.TEXT id));
    (2, (SD.TEXT place));
    (3, (SD.INT time))] in
  sqlite_bind_exec bindings sql_update_user_stmt ;;

let create_user id user time = 
  let bindings = 
  [ (1, (SD.TEXT id));
    (2, (SD.TEXT user));
    (3, (SD.INT time));
    (4, (SD.INT time))] in
  try
    sqlite_bind_exec bindings sql_create_user_stmt 
  with SqliteConstraint -> raise SqliteConstraint ;;
