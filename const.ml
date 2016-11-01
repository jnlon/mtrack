(* Constants / Configuration paramaters *)
let max_upload_len = 10*1024 ;; (*10KB*)
let max_username_length = 32 ;;
let min_username_length = 8 ;;
let db_max_query_rows = 50 ;;
let query_special_enabled = true ;;
let db_file_path = "db/sampledbV3.sqlite" ;;
let sleepy_loop_delay = 300.0 ;;
let id_char_length = 8 ;;
let stale_db_entry_time = Int64.of_int ((60*60*24)*8) ;; (* in seconds *) 
let backlog = 50 ;;
let buffer_size = 20480 ;;
let port = 9944 ;;
let location_config = 
  Location.config_of_file "json/locations.json" ;;
let default_query_response = (`Assoc [("QueryResponse", `List [])]) ;;
