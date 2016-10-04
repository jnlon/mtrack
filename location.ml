(*
 * Server: comparing lattitude/longitude, and returning a location
 *
 * Problem: How do we report either being on campus or off campus? If we mix
 * this data into locations, it's possible to be inside 2 locations at
 * once... We could then multiply their differences, and use whatever is smaller
 * *)

type gps = 
  { lat : float; 
    lon : float; }
;;

(* TODO: Can avoid (area c) mess later by simply putting the area here! *)
type location = 
  { sw : gps ;
    ne : gps ;
    area : float ;
    title : string } ;;

type locations = location list ;;

let sprintf = Printf.sprintf ;;
let die msg =
  print_endline msg;
  exit 1 ;;

let string_of_location loc = 
  let sw,ne = loc.sw,loc.ne in
  sprintf
    "Location: '%s': sw = (lat:%f,lon:%f), ne = (lat:%f,lon:%f), area = %f\n"
    loc.title sw.lat sw.lon ne.lat ne.lon loc.area ;;

let find_float key json =
  try
    Yojson.Safe.Util.to_float (List.assoc key json)
  with _ -> die @@ sprintf "Key '%s' is not a float!" key ;;

let gps_of_latlon_json json_l = 
  { lat = find_float "lat" json_l ;
    lon = find_float "lon" json_l } ;;

let area sw ne = 
  (abs_float ((ne.lon -. sw.lon) *. (ne.lat -. sw.lat))) ;;

let gps_pair_of_json = function
    | `Assoc [(what, `Assoc latlon_data)] -> (what, (gps_of_latlon_json latlon_data)) 
    | _ -> raise Not_found ;;

let location_of_json (data : (string * Yojson.Safe.json)) =
  let place,json = data in 
  match json with
   | `List data ->  (* json array *)
       try 
         begin
           let pairs = List.map gps_pair_of_json data in
           let sw = List.assoc "southwest" pairs in
           let ne = List.assoc "northeast" pairs in
           { sw = sw ;
             ne = ne ;
             area = area sw ne ;
             title = place }
         end
       with Not_found -> die @@ sprintf "Missing fields or malformed data '%s'" place
   | _ -> die @@ sprintf "Invalid json in '%s'" place ;;

let locations_of_json (json : Yojson.Safe.json) = 
  match json with
   | `Assoc [("locations", `Assoc json_locations)] -> 
       List.map location_of_json json_locations
   | _ -> die "No 'locations' in json!" ;;

let config_of_path path = 
  locations_of_json @@ Yojson.Safe.from_file path ;;

let config = 
  let open Printf in
  let config_file_path = 
    try Sys.argv.(1)
    with _ -> "json/locations.json" in
    config_of_path config_file_path ;;

(* TODO!!! Come up with some test cases and test this ... *)

(* lat increase north, lon increase east*)
(* Determines under what building the user is in... 
 * This is put in the DB, and may be queries against. This does not get sent
 * back to the user (they know where they are!)
 * *)

let string_of_locations () =
  (String.concat "" (List.map string_of_location config)) ;;

let title_of_gps gps =
  let in_location_bounds l =
    let ulat,ulon = gps.lat,gps.lon in
    let in_lon_bounds = (ulon > l.sw.lon && ulon < l.ne.lon)  in
    let in_lat_bounds = (ulat > l.sw.lat && ulat < l.ne.lat) in
    if (in_lon_bounds && in_lat_bounds)
    then true
    else false
  in
  let locations = List.filter in_location_bounds config 
  in
  (*let area v = 
    let abs = abs_float in
    (abs_float ((v.ne.lon -. abs v.sw.lon) *. (v.ne.lat -. v.sw.lat))) 
  in*)
  let cmpfun a b =    
    if (a.area < b.area) 
      then -1
      else 1 
    in

  (*List.iter (fun c -> (Printf.printf "%s -> %f\n") c.title c.area) config;
  List.iter (fun l -> Printf.printf "Within bounds of %s\n" l.title) locations;*)

  try
    let smallest_location = List.hd @@ List.sort cmpfun locations in
    smallest_location.title
  with _ -> "Unknown"
;;

let title_of_latlon (latlon : (float * float)) =
  let lat,lon = latlon in
  title_of_gps {lat=lat;lon=lon}
;;

(*List.iter (fun l -> Printf.printf "Within bounds of %s\n" l.title) locations;*)


(*let config = (config_of_path (Sys.argv.(1))) in
List.map (fun c -> print_endline @@ string_of_conf c) config;
print_endline @@ location_title_of_gps config {lat=(0.5);lon=(0.5)};*)

