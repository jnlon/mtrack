## this is only for debugging, not in release build!
{"Query": "all"}
<or>
## Location Query: only works if id exists and is already at this location
{"Query": { id: "11111111", "location": "place"}} 
<or>
## User Query: only works if user id exists. If queried users don't exist,
## return empty array in QueryResponse
{"Query": { id: "11111111", "users": ["my_best_frend", "that_other_guy"]}}
