module String = struct
  include String

  let explode s = 
    let limit = String.length s in
    let rec explode = function
        i when i = limit -> []
      | i -> s.[i] :: (explode (i+1)) in
    explode 0 ;;

  let unexplode ch_lst = 
    let len = List.length ch_lst in
    let b = Bytes.create len in
    let rec unexplode i = function
      | [] -> b
      | ch :: rest -> begin
          Bytes.set b i ch;
          unexplode (i+1) rest
      end
    in Bytes.to_string @@ unexplode 0 ch_lst ;;

end;;


module Unix = struct
  include Unix
  let time_int64 () = 
    Int64.of_float (Unix.time ()) ;;
end;;
