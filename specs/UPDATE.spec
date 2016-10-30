{"Update": 
  { "id": "11111111",
    "gps" : {"lat": 0.5, "lon": 0.5}}}

<or>

# If the username changes, it goes into DB. It only affects username query
{ "Update": 
  { "id" : "11111111",
    "aps" : [ {"ssid" : "blah", "bssid": "0101010101"},
              {"ssid" : "blah1", "bssid": "1111111111"} ] } }
