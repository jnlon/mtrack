(*
 * Server: comparing lattitude/longitude, and returning a location
 *
 * Problem: How do we report either being on campus or off campus? If we mix
 * this data into locations, it's possible to be inside 2 locations at
 * once... We could then multiply their differences, and use whatever is smaller
 * *)

module Yj = Yojson.Safe ;;
open Extensions;;

type gps_coord = 
  { lat : float; 
    lon : float; } ;;

type access_point = 
  { ssid : string;
    bssid : string; } ;;

type gps_location = 
  { name : string;
    sw : gps_coord;
    ne : gps_coord;
    area : float; } ;;

type ap_location =
  { name : string;
    aps : access_point list } ;;

type location_config = 
  { apl  : ap_location list;
    gpsl : gps_location list; }  ;;

let print_json j = 
  print_endline @@ Yj.to_string j ;;

let die msg =
  print_endline msg;
  exit 1 ;;

let string_of_location_conf (loc_cfg : location_config) =
  let sprintf = Printf.sprintf in
  let string_of_ap_location apl = 
    let aps = 
      List.map
        (fun a -> (sprintf "    ssid: %s, bssid: %s") a.ssid a.bssid)
        apl.aps in
    sprintf "AP Location: %s \n%s\n\n" apl.name (String.concat "\n" aps)
  in
  let string_of_gps_location gpsl =
    let sw,ne = gpsl.sw,gpsl.ne in
    sprintf
      "GPS Location %s:\n    sw = (%f,%f), ne = (%f,%f), area = %f\n\n"
      gpsl.name sw.lat sw.lon ne.lat ne.lon gpsl.area 
  in
  let aps_string_l,gps_string_l =
    (List.map string_of_ap_location loc_cfg.apl),
    (List.map string_of_gps_location loc_cfg.gpsl)
  in
  String.concat "" (aps_string_l @ gps_string_l)
;;

let gps_of_json = function
  | `Assoc json_l -> begin
    let to_float = Yj.Util.to_float in
    { lat = to_float @@ List.assoc "lat" json_l ;
      lon = to_float @@ List.assoc "lon" json_l } end 
  | _ -> raise Not_found ;;

let ap_data_of_json = function
  `Assoc json -> begin
    let to_string = Yj.Util.to_string in
    { ssid = to_string @@ List.assoc "ssid" json;
      bssid = to_string @@ List.assoc "bssid" json } end
  | _ -> raise Not_found ;;

let area sw ne = 
  (abs_float ((ne.lon -. sw.lon) *. (ne.lat -. sw.lat))) ;;

let locations_of_json_list (jsonlst : (string * Yj.json) list) =
  let rec parse_json r = function
    | (place, (`Assoc [("gps", `Assoc gpsjson_l)])) :: rest -> begin
      let ne = gps_of_json @@ List.assoc "northeast" gpsjson_l in
      let sw = gps_of_json @@ List.assoc "southwest" gpsjson_l in
      let gps = 
        { name = place ;
          sw = sw ; 
          ne = ne ;
          area = area sw ne } in
        parse_json {r with gpsl = (gps :: r.gpsl)} rest
    end

    | (place, (`Assoc [("aps", `List apsjson_l)])) :: rest -> begin
        let aps = 
          { name = place ;
            aps = (List.map ap_data_of_json apsjson_l) } in
        parse_json {r with apl = (aps :: r.apl)} rest
    end
    | [] -> r
    | _ -> raise Not_found
  in
  parse_json {apl=[];gpsl=[]} jsonlst
;;

let locations_of_json = function
   | `Assoc [("locations", `Assoc json_locations_l)] -> 
       locations_of_json_list json_locations_l
   | _ -> die "No 'locations' in json!" ;;

let config_of_file path = 
  locations_of_json @@ Yj.from_file path ;;

let title_of_gps gps (gpscfg : gps_location list) =
  let in_location_bounds c =
    let ulat,ulon = gps.lat,gps.lon in
    let in_lon_bounds = (ulon > c.sw.lon && ulon < c.ne.lon)  in
    let in_lat_bounds = (ulat > c.sw.lat && ulat < c.ne.lat) in
    if (in_lon_bounds && in_lat_bounds)
      then true
      else false
  in
  let locations = List.filter in_location_bounds gpscfg 
  in
  let cmpfun a b =    
    if (a.area < b.area) then -1 else 1 
  in
  try
    let smallest_location = List.hd @@ List.sort cmpfun gpscfg in
    smallest_location.name
  with _ -> "Unknown" ;;


(* When Given the user's surrounding APs, return the location with the most
 * matching APs compared to the user's *)
let title_of_aps (aps : access_point list) (apscfg : ap_location list) =

  let points_of_location loc = 
    let points = 
      List.fold_left
      begin fun sum usrap ->
        sum + (List.count usrap loc.aps)
      end 0 aps
    in
    (loc.name, points)
  in

  let points = 
    List.map points_of_location apscfg 
  in

  let cmpfun a b =
    if (snd a) < (snd b)
    then 1
    else -1
  in

  fst @@ List.hd @@ List.sort cmpfun points
;;

let title_of_latlon (latlon : (float * float)) gpscfg =
  let lat,lon = latlon in
  title_of_gps {lat=lat;lon=lon} gpscfg ;;

let title_of_netids (netids : (string * string) list) gpscfg =
  let aps = List.map (fun n -> {ssid=(fst n);bssid=(snd n)}) netids in
  title_of_aps aps gpscfg ;;

let sample_run () = 
  let location_config = config_of_file "json/locations.json" in
  (*print_endline @@ string_of_location_conf location_config;*)
  (*print_endline @@ title_of_netids [("eduroam", "04:da:d2:4f:36:ae")] location_config.apl*)
  print_endline @@ title_of_aps [{ssid="MacSecure"; bssid="ac:a0:16:bb:16:20"}] location_config.apl
;;

sample_run () ;;
