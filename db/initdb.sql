.open sampledbV3.sqlite
CREATE TABLE user_locations (
  userid TEXT NOT NULL UNIQUE CHECK(length(userid) = 8),
  username TEXT NOT NULL UNIQUE CHECK(length(username) <= 32 AND length(username) >= 8),
  location TEXT NOT NULL DEFAULT '' CHECK(length(location) <= 500),
  last_update_time INTEGER NOT NULL DEFAULT 0,
  creation_time INTEGER NOT NULL DEFAULT 0
);

-- Userid: 
  -- Specific to the app installation. Created in response to the API "Create"
  -- method, which allows the server to ensure that it is unique 

-- Username:
  -- How users are able to query other users they know
  -- This can be changed in client by making another Create request

-- Location:  
  -- Where the user currently is, updated with Update APS and Update GPS, value
  -- is calculated by the server, uploaded data is discarded
  -- Used to determine if a user is in the same location as the user who called
  -- Query with Location.
  -- Appears on UI, received from query requests  

-- last update time
  -- used to determine if the user is still active
  -- used to sort on user query

-- Creation time
  -- time Create was called... Not used?
