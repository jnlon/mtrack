# Returned to the client in response to Query
{ "QueryResponse": 
  [
    {"username": "sample", "location": "someplace", "lastupdate": 1232},
    {"username": "me", "location": "home", "lastupdate": 1233},
    {"username": "admin", "location": "nowhere", "lastupdate": 12123},
    {"username": "root", "location": "/var/empty", "lastupdate": 12123},
    {"username": "wheel", "location": "no", "lastupdate": 12123}
  ]
}

# Note that on startup, to confirm if the app users profile still exists, a
# self-user-query will be done. If this returns empty, then we are going to
# request a new ID / username with Create. A user profile may become deleted if
# they have not made any requests for a long period of time
