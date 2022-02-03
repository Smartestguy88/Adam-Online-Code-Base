-- This script was written by Smartest_guy8
-- GG gl

print("MASTER SERVER SCRIPT -- LOADED")

local cc = "init" -- Code context, for debugging
local fc = "<unspecified>"     -- Function context, for debugging
-- Copy below code to add generally a good context to any string: (without comment parts :)
-- .. "; In: " .. cc .. "/" .. fc or "Unstated function context"

-- Code version is now handled by Github, https://github.com/Smartestguy88/Adam-Online-Code-Base/blob/main/SERVER%20MASTER%20SCRIPT.lua
local codeVersion = script:GetAttribute("codeVersion") or "v-Absolute-1.0.3" -- Latest code version updated in attribute
local codeName = "[" .. script.Name or "MASTER SERVER SCRIPT-Absolute" .. "]" -- Should result in "[MASTER SERVER SCRIPT]"
local dataStoreName = "__" .. "Data Store::" .. tostring(codeVersion) -- Used to access dataStore, change this var to stop resets per code version

-- Code verion history
--[[
Code version history (git not used as I hope it will never have to be) # Lol written before this is all on git :)
v1.0.0 :: Created
v1.0.1 :: Contains overrided (in development) type, assert, and is functions // Send on discord
v1.0.2 :: WORKING OBJECT INHERITANCE SYSTEM LETS F**KING GO 7 HORUS OF CODING STRAIGHT
--]]

do -- Assert basic script vars
  codeVersion = tostring(codeVersion)
  codeName = tostring(codeName)
  dataStoreName = tostring(dataStoreName)

  assert(codeVersion)   -- Code must have a version
  assert(codeName)      -- Code must have a name
  assert(dataStoreName) -- Code must have the relative data store name
end

-- Everything to do with communication
--[[

  **Everything to do with data stores:**

  There is only ONE ROOT data store: '_DataStore::v1.0.0' // Will be updated as versions
  This data store follows a naming convention that goes like this:

  // Note: Naming convention is applied per entry
  // E.g. Player / INDEX / Leaderboards / Gold is written as 'Player/INDEX/Leaderboards/Gold' which is the name of an entry
  <Data Store>:
    Player /
      # Everything concerning a player, all info needed by a client or server concerning a client
      # This should be updated around once/twice a minute in total per player
      INDEX /
        # IMPORTANT
        # Managed by the *BOSS* server
        Leaderboards /
          Gold ? Silver ? Metal etc
          --> LeaderboardObject [PlayerID = Gold ? Silver ? ... ?]
        Structures /
          MapPlaced --> [StructGridObject]
          StructureIndex --> [Structure IDs]
      Persistent /
        # All data pertaining to a specific player
        # Linked through PlayerID<Instance> object (Players.LocalPlayer.UserId)
        Items  /
          # All types of items
          Gold --> Player ID --> ResourceObject
          Silver "  "
          Metal  "  "
          Blueberries
          e.t.c.
          # Each entry represents a type of item of a specific player
          # Each entry contains a custom table/object
        Communication /
          Chat --> Player ID --> ChatObject {ChatID: ChatThread}
          Group -> Player ID --> ChatObject {ChatID: ChatThread}
          Alliance
          Random
          INTERNET??
          etc
        Structures /
          # This section is IMPORTANT as it is an index of all structures a player owns
          # This section, for any important events checks DataStore/Global/Structure/Index/<Requested ID> as superior
          # It also allows for history as doesn't necessarily need to update
          History --> Player ID --> [Structure IDs]
          Current --> Player ID --> [Structure IDs]
        Settings /
          Player ID --> SettingsObject { ... }
        History /
          Play Time Leaderboard --> INDEX
          Player ID --> HistoryObject{ ServerID<Instance> = CustomCacheDataType<table>, etc }
      Global /
        # Concerning data that ALL players can see / interact with
        # This data is EXTREMELY IMPORTANT
        # Methods will be put into place to ensure that only one server
        # can edit this data at a time because corruption and/or mis-editing
        # can lead to DISASTROUS consequences
        Structures /
          Structure ID --> StructureObject {parts, partNum} # IMPORTANT
        Chat INDEX /
          # This index is used to access raw ChatThread objects which contain the actual data for a chat
        Internet ???
      Debug /
        # Used for communicating debug info from multiple servers: Multi-Server Debugging (below)
        # See also: Message Channel: 'MultiServerDebugging' and 'MultiServerLog'
        Server ID<Instance> --> Random Info
      Admin /
        # Used for communicating to individual servers specific info from any server
        # 
        Server ID<Instance> --> Random Info
      ServerManagement /
        # This data store section is used to determine the boss server and to manage its communication channels
        BOSS SERVER ID
        TOOMANYBOSSES
        # INDEX will be kept by the boss server as a record of all servers that need to be communicated with
        INDEX --> [Server ID<Instance>] // Managed by boss server
        Final Cache --> Server ID<Instance> --> Custom Cache Data Type<table> # Plebs post here under their own id to commuunicate with the boss server

  To make sure that data is not corrupted, Data Store Service and Messaging Service will be used to communicate between servers
  to decide which one is boss and which ones are plebs
  This means that no data is updated without taking into account the changes applied by another server at the same time

  *Limite of Data Store Service (In seconds, per minute):*
    -- Sixe of entry: 1MB (MegaByte)
    -- Get: 60 + (numPlayers × 10) per server
    -- Set: 60 + (numPlayers × 10) per server
    -- Set the same key: [6]
    -- Get Keys: 5 + (numPlayers × 2) per server

  *Limits of Messaging Server (In number, per minute):*
    -- Size of message: 1kB (KiloByte)
    -- Send: 150 + (60 × numPlayers) per server
    -- Receive: 100 + [50 × numServers] per game
    -- Receive per topic: 10 + [20 × numServers] per topic

  *Each PLEB server can (per server):*
    -- Set: 10 per minute per player
    -- Get: 10 per minute per player
    -- Get Keys: 2 per minute per player
    -- Send: 60 per minute per player
    -- Receive: 50 per minute
    -- Receive: 20 per minute per topic

  *Each BOSS server can (per *BOSS* server):*
    -- Set: 60 per minute
    -- Get: 60 per minute
    -- Get Keys: 5 per minute
    -- Send: 150 per minute
    -- Receive: 100 per minute
    -- Receive per topic: 10 per minute
  
  System requirements:
  *What this system needs to do:*
  -- Allow only 1 BOSS server
  -- Allows the BOSS server to handle **MANY** requests
  -- Allows INFINITE other servers to submit their requests to alter IMPORTANT info
  -- Allows a safe catch if any server, esspecially the boss server, is shut down
  -- Allows a safe catch if any server, esspecially the boss server, is shut down WITHOUT WARNING

  The only way a system could be realistically created is if a slight margin of error is accepted:
  -- If the boss server, while accessing data, is shut down WITHOUT WARNING the data being accessed may be corrupted
  -- It is acceptable to assume that no more than 10 servers will shutdown without warning
  -- Assumes 5 second between leaving server and joining new one

  1.) When a server first joins, it checks DataStore/ServerManagement/BOSS SERVER ID to see if it is empty
  2.) If it is not than the server will now consider that server boss
  -- Stop if step 2 is reached :: This server is now a *PLEB*
  3.) If it is than it will save the current time and check TOOMANYBOSSES to see if it is empty or has recorded a later time than saved
  4.) If it is than it will save the current time and its id and wait 11 seconds (enough time for datastore updates to push through for real)
  5.) If after that time this servers ID and time or a time after this saved one is still there AND a boss id is not saved, become a boss yay!
  -- Stop if step 5 is reached :: This server is now *BOSS*
  6.) If TOOMANYBOSSES recorded time is after this servers saved time or a boss server is already decided than wait until DataStore/ServerManagement/BOSS SERVER ID is set and use that as boss
  -- Stop if step 6 is reached :: This server is now a *PLEB*

  **Multi-Server Client Model:**
  This game, whatever its current name is, was originally designed to mimick the data model behind Eve Online
  To be more specific, it was intended to have two data models in use:
  > The Roblox Data Model, where data is stored per player and each player has data specific to them
      This data model is typical across Roblox games, think of an inventory of items which each player has and is unique and persistent to that player
  > The MMO or Eve Data Model, where data is stored per game and each player has data in relation to everybody else
      This data model is NOT typical of Roblox games, which is one reason I wanted to create such a game: To be the first

  These models are implemented as a SINGLE DATA STORE using Roblox's Data Store Service
  The reason only one data store is used is such that the absolute names of data stores are not required
  This means resetting data, for any reason, is as simple as changing the name of the data store used as the main store and resetting all the servers
  Another reason only one data store is used is such that the cache (temporary memory) of async functions used to interact with the data store is not corrupted when changing any data
  https://developer.roblox.com/en-us/articles/Data-store#caching
  See above site for a justification, the red section/box and the section below it is why

  There are limitations to the Roblox's Data Store Service (as described in the above table), the most obvious being a 4 MB limit to each entry and a time limit on requests
  This time limit is the reaons that I have implemented the idea of a *BOSS* server, which manages IMPORTANT index entries
  INDEX entries are entries that are typically arrays of ID references for the purpose of keeping tabs on a section of datastore that gives meaning to each ID
  Their are currently (time of writing) two main uses for these INDEX entries: leaderboards and a server loading all structures
  A leaderboard is simply a representation of data about a list of players which needs to be in order, and when a server loads in all structures it needs to with as much speed and least network effort render all structures which INDEX type entries solve
  Note: INDEX entries are no different from any other entry except for their data type and how they are treated EXCEPT for this *important* fact:
  They are updated by the *BOSS* server such that the indexes aren't corrupted by many servers attempting to edit the same key many times and
  they are automatically updated when another related entry is editted when it makes sense, for example:
  ... Gold --> Player ID --> ResourceObject(...data...)
  Could logically be linked with a leaderboard of players to see who has the highest amount of Gold:
  ... INDEX / Leaderboards / Gold
  This means that whenever a player's amount of gold is editted a logical check is performed to see if the player should make it onto the leaderboard, which displays the link between an index and another entry value

  **Multi-Server Debugging:**
  Multi server debugging is achieved through the use of shared Message Service channel named "MultiServerDebugging"
  This is only subscribed to if a script level error occurs, and then a message is pushed to this topic explaining how this occured
  Multi server debugging is also achieve through the use of a shared Message Service channel named "MultiServerLog"
  This is subscribed to by all servers who are undergoing identity crisis and don't know who is boss
  All servers also keep a local record of this info to make it easier to debug

  **In-Server Debugging:**
  In server debugging is done through an admin communicating to the MASTER SERVER SCRIPT while in the offending server
  Servers automaticcally keep an extensive record of all non-multi-server actions that it undertakes to allow for easier debugging
  Only admins, such as myself, can access this data and use the raw functions available to aforementioned admins
  All indeterministic return typed functions (functions that may return a fail in raw or processed action requested) return an IRTF object

  **Server-Client Communication**
  TO REMEMBER: ONLY NUMBERED INDEXES IN TABLES PASSED TO REMOTE FUNCITONS ARE SEEN, see:
  https://developer.roblox.com/en-us/articles/Remote-Functions-and-Events#mixed-tables

--]]

-- Object Hierarchy
--[[
  ** Object Inheritance **
  Objects all stem from one object, namely Object (or _G["Object"] for absolute reference)
  Object inheritance is, for the sake of simplicity and definite __index references, single parent many children
  As in, one table cannot 'inherit' from multiple parents, although this is most definitely possible

  **Ideal Object Hierarchy:**
  The ideal object hierarchy would include:
  Server type object
  Multiserver type object (protocol for accessing data outside this server)
  Client object to deal with client requests
  Communications object to oversee all communications (in and outside of this server)

  **Actual Object Hierarchy**
  Object:
    CoreTypes:
      Async: -- Allows queing of yeilding functions and callbacks
      Debug: -- Debug functions like printing to console
      Server: -- Holds all info about a server
      
--]]

-- Basic formatting guide to this program:
--[[

  This program is split in two major ways:

  -> cc :: Code Context, which divides the program into major sections indicated by lines of hyphens '-'
  --> mc : Minor Context, which divides the program into minor sections indicated by a space
  -> fc :: Function Context, which indicates a function

  cc and fc are both variables that can be access to give a rough indication of where in the program you are
  note: fc is local and only accessable in defined functions, where as cc is global and updated per major section absolutely
  note: if the first line after definition is not local fc = .. then that function doens't have an absolute defined fc

  Major sections, each cc, are demoted by lines of hyphens '-' which conveniently comment themselves out
  Minor sections, each mc is seperated by a space between chunks of code
  mc is NOT a variable, but can be found after the slash '/' in cc

--]]

--------------------------------------------------------------

cc = "global defs"

do -- Global defs (in do for easy collapse) 
  cc = "global defs/metatable"
  
  is = function(obj) -- Used to determine if a table is a custom class (using its __type prop)
    local types = {}
    local trace = {} -- Stops infinite recursion
    local function addType(obj)
      local tp = type(obj)
      if (tp ~= "nil") and (types[tp] == nil) then types[tp] = true end
      if type(obj) == "table" then
        if obj.__type ~= nil then types[obj.__type] = true end -- Add custom type
        if trace[obj] == nil then
          trace[obj] = true  
          if type(getmetatable(obj)) == "table" then addType(getmetatable(obj)) end -- Include the types of the metatables (parents) of the object
        end
      end
    end
    addType(obj)
    return types
  end
  metamethods = { -- Table of all property names that are considered metamethods (copied to ensure proper inheritance)
    ["__add"] = true, ["__sub"] = true, ["__mul"] = true, ["__div"] = true, ["__mod"] = true, ["__pow"] = true, -- Mathematical operators
    ["__unm"] = true, -- Used like '-table'
    ["__concat"] = true,  -- Used like 'table .. table'
    ["__len"] = true, -- Used like '#table'
    ["__eq"] = true, ["__lt"] = true, ["__le"] = true, -- Used like 'table == table' or 'table >= table' or 'table > table' etc
    ["__call"] = true, -- Used like table()
    ["__gc"] = true, -- Garbage collected
    ["__tostring"] = true, -- Used like tostring(table); note print automatically uses this func
    ["__newindex"] = true, -- Used like table["newEntryName"] = ...
    -- Below 'metamethods' are custom, they are treated as such and inherited
    ["__default"] = "USER DEFINED", -- Represents default entry to add to table when instinating with :New()
    ["__name"] = "USER DEFINED", ["__type"] = "USER DEFINED", -- Represents info of the table for debugging and tostring purposes
  }
  validMetatable = function(obj) -- Checks whether an object has a valid (accessable) metatable
    assert(type(obj) == "table")
    return type(getmetatable(obj))  == "table"
  end
  vm = validMetatable
  
  cc = "global defs/absolute debugging" -- fc not required for absolute debugging as, well, fc is FOR absolute debugging :)
  local assert = function(check, message, level, ...) -- Allows for error level when handling check (... optional error args)
    -- _G['assert'] (old version) this code does better because more control over error parameters passed
    if check == "<!is new!>" then
      return true -- Easy test for a program to tell if its dealing with the new assert
    end
    check = check or false
    message = message or "Error caught"
    -- assert(type(message) == "string") -- Error mesages can be ANY type so this is actually inhibiting
    level = level or 0
    if type(level) ~= "number" then
      error("level in assert NaN", 2)
    end
    level = level + 1 -- Level 2 = call pos of assert()
    if not (level >= 1) then
      error("level in assert not >=1", 2)
    end
    if not check then
      error(message .. " \nOther Data: " --[[.. defaultToString({
        message = message,
        check = check,
        level = level,
        args = {...},
      }) ]], level) -- Could possibly cause infinite stack error if defaultToString is called incorrectly
      -- error({message=message, level=level, args={...}, traceback=debug.traceback()}, level)
    else
      return check -- Returns truthy statement
    end
  end
  pe = function(val, message, level, ...) -- Allows for easy Parameter Error/Exists checks e.g. pe(paramInput, "PI has no value")
    -- pt does dynamic below code
    -- assert(val, "No value passed to pe", 2) -- Stupid statement because it can't handle val == nil LOL
    level = level or 2 -- Defaults to blame error on function that called the function that (badly) called pe()
    assert(type(level) == "number", "Level passed to pe NaN", 2)
    level = level + 1 -- Passes error to position of pe() being called
    
    message = message or "Error in parameter caught"
    
    assert(type(val) ~= "nil", message, level, ...)
    return val
  end
  pt = function(val, result, message, level, ...) -- Alows for easy Parameter Type checks e.g. pt(param, "number", "p NaN")
    -- pm does dynamic below code
    local result, reversed = string.gsub(pe(result or "table", "result in pt DNE", 3), "^!not!", "") -- IS there a !not! at the start?
    assert(type(result) == "string", "result in pt not type string", 2)
    local message = pe(message or "", "message in pt DNE")
    local level = pe(level or 2, "level in pt DNE")
    assert(type(level) == "number", "level in pt NaN", 2)
    
    level += 1
    
    local args1 = ...
    local DNE = (is(args1)["table"]) and (args1.DNE) and (result ~= "nil")
    if DNE then pe(val, DNE, level)  end -- Extra message if value doesn't exist if provided in {...}[1].DNE
    
    local orStart, orFinish, index = string.find(result, "!or!") -- Extract !or! statement (singular)
    if reversed % 2 == 0 then
      if orStart then
        print("!OR! DETECTED YAY! 1:'" .. string.sub(result, 1, orStart - 1) .. "' 2:'" .. string.sub(result, -1, orFinish + 1) .. "' ;")
        assert(is(val)[string.sub(result, 1, orStart - 1)] or is(val)[string.sub(result, -1, orFinish + 1)], message, level, ...)
      else
        assert(is(val)[result], message, level, ...)
      end
    else
      if orStart then
        assert(not (is(val)[string.sub(result, 1, orStart - 1)] or is(val)[string.sub(result, -1, orFinish + 1)]), message, level, ...)
      else
        assert(not is(val)[result], message, level, ...)
      end
    end
    
    local sig = {paramName = "unset (no pm added)", paramType = result, context = "unset (no pm added)", value = val, extra = {...}}
    if is(args1)["table"] and args1["pm added:"] then
      local pmAdded = args1["pm added:"]
      sig = {paramName = pmAdded["paramName"], paramType = pmAdded["type"] or result, context = args1["context"]}
    end
    -- Potentially cast types here?
    -- Return below to allow this step to be implemented here without absolute code changes  
    return val, sig
  end
  pm = function(...) -- Maps n params to their specified types and returns them OR raises error depending on correct/incorrect param-type mapping
    local args = pe({...}) -- Unneded pe but used as extra catch
    local params = pt(args[1], "table", "Params index passed to pm not type table", 2)
    local types  = pt(args[2] or {}, "table", "Types index passed to pm not type table")
    local paramNames = pt(args[3] or {}, "table", "paramNames passed to pm not type table")
    local contextInfo = pt(args[4] or {"Unstated function", "Unstated context"}, "table", "Context passed to pm not type table")
    local extra = pt(args[5] or {{}, ["pm added:"] = {}}, "table", "extra passed to pm is not type table ??")
    if type(extra[1]) ~= "table" then
      if extra[1] ~= nil then warn("Overriding first optional parameter provided to pm", debug.traceback()) end
      extra[1] = {}
    end
    if type(extra[1]["pm added:"]) ~= "table" then
      if extra[1]["pm added:"] ~= nil then warn("Overriding 'pm added:' optional parameter provided to pm", debug.traceback()) end
      extra[1]["pm added:"] = {}
    end
    if type(contextInfo[1]) ~= "string" then
      if contextInfo[1] ~= nil then warn("Overriding contextInfo[1] optional parameter provided to pm", debug.traceback(), "from ::", type(contextInfo[1])) end
      contextInfo[1] = "Unstated Function"
    end
    if type(contextInfo[2]) ~= "string" then
      if contextInfo[2] ~= nil then warn("Overriding contextInfo[2] optional parameter provided to pm", debug.traceback(), "from ::", type(contextInfo[2])) end
      contextInfo[2] = "Unstated Context"
    end
    local r = {}
    local n = 0
    for i, v in pairs(params) do
      if type(i) == "number" then
        n += 1
        extra[1]["pm added:"]["index"] = {i = i, v = v} -- Extra info for debugging params passed
        extra[1]["pm added:"]["paramName"] = paramNames[i]
        extra[1]["pm added:"]["type"] = types[i]
        extra[1]["pm added:"]["context"] = contextInfo[i]
      end -- Don't count arguments towards unpack total if they not numerical
      types[i] = types[i] or "!not!nil" -- Default argument is not nil '!not!nil'
      extra[1].DNE = tostring(paramNames[i]) .. " does not exist in " .. contextInfo[2] .. "/" .. contextInfo[1]
      
      r[i] = pt(params[i], types[i],
        "Bad argument #"..i.." "..tostring(paramNames[i])..": Not type "..types[i].." :( "..contextInfo[2] .."/:".. contextInfo[1]..";",
        3,
        extra
      )
    end
    local sig = {}
    for k, v in pairs(params) do
      sig[paramNames[k] or k] = {v = v, is = types[k]}
    end
    n += 1 -- Allow for signature in return parameters
    table.insert(r, n, sig)
    return unpack(r, 1, n)
  end

  cc = "global defs/string"
  printLogs = false
  tostringGen = function(specifications) -- Generates a tostring function according to specifications
    local fc = "tostringGen"
    assert(type(specifications) == "table")
    local indentations, lineFormatting, beginning = specifications.indentations, specifications.lineFormatting, specifications.beginning
    assert(type(indentations) == "table")
    assert(type(lineFormatting) == "table")
    assert(type(beginning) == "table")
    local firstIndent, noramlIndent, metaIndent = indentations.first, indentations.normal, indentations.meta
    local suffix, blacklist = lineFormatting.suffix, lineFormatting.blacklist
    local wrapper = beginning.wrapper
    local 
  end
  defaultToString = tostringGen("> ", "==", ",") -- The default tostring method for printing to the console (raw)

  cc = "global defs/table manipulation"
  forceContain = function(obj, contains, index, override) -- Used to easily force obj into a default pattern (no overriting) with optional index for limiting affected entry of obj
    local fc = "forceContain"
    local obj, contains, override, _ = pm({obj, contains, override}, {"table", "table"}, {"obj", "contains"}, {cc, fc})
    local o
    if index ~= nil then
      o = obj[index]
    else
      o = obj
    end
    for k, v in pairs(contains) do
      if override or o[k] == nil then o[k] = v end -- If overriding allowed then force o to conform to o[k]=v
    end
    return obj
  end
  
end

do -- Global reference names  
  Players = game:GetService("Players")
  ReplicatedStorage = game:GetService("ReplicatedStorage")

  DataStoreService = game:GetService("DataStoreService")
  StandardDataStore = DataStoreService:GetDataStore(dataStoreName)
end

--------------------------------------------------------------

cc = "object hierarchy"

do -- Object hierarchy (in do for easy collapse)
  cc = "object hierarchy/Object def"
  
  -- See above for object hierarchy tree description
  Object = {
    __type = "object", -- This is a type (duh)
    __name = "[Object:( Inherited]", -- Indicated inherited because Object is not considered static
    creation = {
      raw = "Inherited from Object :(", -- Will contain the raw time given by os.time() (see Creation below)
    },
    Log = {
      [0] = "Inherited from Object :("
    },
    log = function(self, ...)
      local fc = "Object/log"
      local args = {...}
      self = pt(self, "object", "self passed to " .. fc .. " in " .. cc .. " not type object")
      if #args == 1 then
        args = pt(args[1], "table", "arg 1 passed to " .. fc .. " in " .. cc .. " not type table")
      end
      args.t = args.t or args.type or args[1]
      args.m = args.m or args.message or args[2]
      local extra = {}
      for i, v in ipairs(args) do
        if i >= 3 then
          extra[i] = v
        end
      end
      args.e = args.e or args.extra or extra

      local t = args.t -- Type function/log/debug/state
      local m = args.m -- Message typically text
      local e = args.e -- Extra info
      
      -- Custom actions like debug printing achieved here
      if t == "debug" then
        print("Debug message logged: ", m, e)
      end
      
      self.Log = rawget(self, "Log") or {} -- Ensures 'unique' log for each instance
      assert(self.Log, "table", "self.Log caught not being a table :( in " .. cc .. " in " .. fc .. ";")
      table.insert(self.Log, {type = t, message = m, extra = e})
    end,
    Creation = function(self)
      local fc = "Object/Creation"
      self = pt(self, "object", "self passed to " .. fc .. " in " .. cc .. " not type object")
      self:log{"function", fc, cc}
      return {
        raw = os.time(),
      }
    end,
    New = function(self, ...) -- One intended arg: Object constructor used Object:New{abc = 123}
      local fc = "Object/New"
      local self, sig = pt(self, "table", "self passed to New in Object is not type table ??" .. "; In: " .. cc .. "/" .. fc)
      local default = {} -- THE reference to the final object
      local function getDefault (obj)
        if validMetatable(obj) then
          getDefault(getmetatable(obj)) -- Runs __default from lowest to highest object recurssively
        end
        if type(rawget(obj, "__default")) == "function" then
          forceContain(default, obj:__default(), nil, true) -- Runs default on metatable
        end
        return default
      end
      -- Time of creation calculated here (if __default() is applicable to that nature)
      local args = forceContain({...}, getDefault(self), 1, false)
      -- Default param to automatically add an absolute instination time of Object instance for debugging (to index [1], =o)
      local o = args[1] or default -- Contains default creation property guarenteed, assert below for debugging
      pt(o, "table", "For some reason o is not a table ??" .. "; In: " .. cc .. "/" .. fc, 1)
      local mt = self -- Must have __index pointing to self (next line, implied)
      -- self.__index = self -- Unnecessary but implied/assumed (done absolute below Object definition)
      -- Below for loop copies from the metatable of mt to mt such that o's metatable contains the correct metamethods (__index useless because rawget in lua source code)
      if validMetatable(mt) then -- If mt has a valid metatable copy from it to mt itself
        for k, v in pairs(getmetatable(mt)) do
          if metamethods[k] ~= nil and rawget(mt, k) == nil then
            rawset(mt, k, v) -- Copy metamethods from getmetatable(self) to mt such that the new object has correct access
          end
        end
      end
      o.__index = o -- Makes sure o starts with correct __index on self, Object is exception so done absolute (below definition)
      setmetatable(o, mt) -- o now has metatable with __index pointing such that it looks to self (Object) as 'parent' inherited
      
      self:log{"function", tostring(self.__name) .. " creating :New() object with name " .. tostring(o.__name), fc, cc}
      return o -- o will now reference Object (or self if this param is inherited/changed) in __index
      -- A question ANSWERED by me:
      -- Say a parent object had a property that was 'static', as in it was defined by it as was UNIQUE to that object and NOT its children
      -- For example, time of instination. Does a child of that parent object inherit a reference to that same unique field and so
      -- when updating its own unique version update the parents field instead?
      -- ANSWER: NOOOO
      -- Retrieving using __index will RETRIEVE the parent but setting any objects properties will make that property UNIQUE to ONLY that child
      -- Which means that the 'reference' to the property is only for retrieving, not updating
    end,

    -- Below are functions used as part of the Objects metatable (because Object considered non-static it has no metatable)
    __tostring = tostringGen("> ", "==", ","),
    __default = function(self)
      local fc = "Object/__default"
      self, sig = pm({self}, {"table"}, {"self"}, {cc, fc})
      self:log{"function", fc, cc, sig}
      return {
        creation = self:Creation(),
      }
    end,
    --[[ Will cause infinite stack errors as the tostring function uses __concat a bit
      __concat = function(a, b) -- Is the cause for a few infinte stack errors :)
        local fc = "Object/__concat"
        --local a, b = pm({a, b}, {}, {"a", "p"}, {cc, fc}) -- May cause infinite stack error
        return tostring(a) .. tostring(b)
      end,
    --]]
  }
  Object.__index = Object -- Absolute object reference
  
  cc = "object hierarchy/Core types def"
  Server = Object:New{ -- *PARENT* (NOT INSTANCE) of all server related objects NOTE: ONLY EXISTS ON LOCAL SERVER
    __type = "server",
    __name = "[server inherited]",
    threads = false,
    _RuntimeHandler = RUNTIME, -- Reference to global variable defined in cc = "object implementation"
    __default = function(self)
      local fc = "Server/__default"
      self = pt(self, "server", "self passed to " .. fc .. " in " .. cc .. " not type table")
      local r = {}

      r.__type = "serverobject inherited"
      r.__name = "[server object :( inherited]"

      -- Queues this for potential triger functions like 'instinate' or 'terminate' that need to be handled by the local RuntimeHandler (if threads)
      if r.threads then self._RuntimeHandler:queueForInspection(r) end
      return r
    end,
  }

  RuntimeHandler = Server:New{
    __type = "runtimehandler",
    __name = "[Runtime Handler :( inherited]",
    threads = false,
    __default = function(self)
      local fc = "RuntimeHandler/__default"
      self = pt(self, "runtimehandler", "self passed to " .. fc .. " in " .. cc .. " not type runtimehandler")
      local r = {}

      r.perStep = {}
      r.start = {}
      r.terminate = {}

      r.subscribeStep = function(self, obj)
        fc = "RuntimeHandler/__default/subscribeStep"
        self, sig = pt(self, "runtimehandler", "self passed to " .. fc .. " in " .. cc .. " not type runtimehandler")
        self:log{"function", fc, cc, sig}
        table.insert(r.perFrame, obj)
        self:log{"debug", "Just subscribed an object to step RUNTIME permissions", obj, fc, cc, sig}
        return obj
      end
      r.subscribeStart = function(self, obj)
        fc = "RuntimeHandler/__default/subscribeStart"
        self, sig = pt(self, "runtimehandler", "self passed to " .. fc .. " in " .. cc .. " not type runtimehandler")
        self:log{"function", fc, cc, sig}
        table.insert(r.perFrame, obj)
        return obj
      end
      r.subscribeTerminate = function(self, obj)
        fc = "RuntimeHandler/__default/subscribeTerminate"
        self, sig = pt(self, "runtimehandler", "self passed to " .. fc .. " in " .. cc .. " not type runtimehandler")
        self:log{"function", fc, cc, sig}
        table.insert(r.perFrame, obj)
        return obj
      end

      r.executeStep = function(self) -- Called every 'step' > 
        fc = "RuntimeHandler/__default/executeStep"
        self, sig = pt(self, "runtimehandler", "self passed to " .. fc .. " in " .. cc .. " not type runtimehandler")
        self:log{"function", fc, cc, sig}
        for k, v in pairs(self.perFrame) do
          local success, _ = pcall(function () return v() end) -- Attempt to call functions/tables with __call
        end
      end
      r.instinateStart = function(self) -- Called when the program 'starts' > 
        fc = "RuntimeHandler/__default/instinateStart"
        self, sig = pt(self, "runtimehandler", "self passed to " .. fc .. " in " .. cc .. " not type runtimehandler")
        self:log{"function", fc, cc, sig}
        for k, v in pairs(self.start) do
          local success, _ = pcall(function () return v() end) -- Attempt to call functions/tables with __call
        end
      end
      r.beginTermination = function(self) -- Called when the program 'ends' > 
        fc = "RuntimeHandler/__default/beginTermination"
        self, sig = pt(self, "runtimehandler", "self passed to " .. fc .. " in " .. cc .. " not type runtimehandler")
        self:log{"function", fc, cc, sig}
        for k, v in pairs(self.terminate) do
          local success, _ = pcall(function () return v() end) -- Attempt to call functions/tables with __call
        end
      end

      r.__call = function(self)
        fc = "RuntimeHandler/__default/__call"
        self, sig = pt(self, "runtimehandler", "self passed to " .. fc .. " in " .. cc .. " not type runtimehandler")
        self:log{"function", fc, cc, sig}
        self:executeStep()
      end
      return r
    end,
  }

  CommunicationObject = Server:New{ -- Parent of all communication related objects
    __type = "communicationobject",
    __name = "[communicationobject inherited]",
    __default = function(self)
      local fc = "Communication/__default"
      self = pt(self, "server", "self passed to " .. fc .. " in " .. cc .. " not type table")
      local r = {}

      r.__type = "communicationobject inherited"
      r.__name = "[communication object :( inherited]"
      return r
    end,
  }
  AsyncObject = CommunicationObject:New{ -- Represents an async request that needs to be handled
    __type = "asyncobject",
    __name = "[asyncobject inherited]",
    requestTypes = {
      ["<default>"] = false, -- Not a valid request type for all intents and purposes
      ["<boss server>"] = 1,
      ["<local data store>"] = 2,
      ["<client>"] = 3,
    },
    requestTypesIndex = {
      [false] = "<default>",
      [1] = "<boss server>",
      [2] = "<local data store>",
      [3] = "<client>",
    },
    __default = function(self)
      local fc = "AsyncObject/__default"
      self = pt(self, "asyncobject", "self passed to " .. fc .. " in " .. cc .. " not type table")
      local r = {}

      r.__type = "asyncobject inherited"
      r.__name = "[async object :( inherited]"
      r.requestType = self.requestTypes["<default>"]
      r.index = false -- Default
      r.callback = false -- Default
      return r
    end,
  }
  AsyncHandler = CommunicationObject:New{ -- Object that holds and is called periodically to handle async requests from LOCAL server
    __type = "asynchandler",
    __name = "[asynchandler inherited]",
    threads = true, -- Means this object is queued for the local runtime handler
    __default = function(self)
      local fc = "AsyncHandler/__default"
      self = pt(self, "asynchandler", "self passed to " .. fc .. " in " .. cc .. " not type asynchandler")
      local r = {}

      r.__type = "asynchandler inherited"
      r.__name = "[async handler :( inherited]"
      r.asyncRequests = {}
      r.appendRequest = function(self, requestObj)
        local fc = "AsyncHandler/__default/appendRequest"
        self, requestObj, sig = pm({self, requestObj}, {"table", "asyncobject"}, {"self", "requestObj"}, {fc, cc})
        self:log{"function", fc, cc, sig}
        table.push(self.asyncRequests, requestObj)
        return requestObj
      end
      r.typeMapping = { -- Maps preset type names (e.g. <client>) to the ABSOLUTE FUNCTION NAME e.g. handleClient  > 
        ["<default>"] = "handleDefault",
        ["<boss server>"] = "handleBossServer",
        ["<local data store>"] = "handleLocalDataStore",
        ["client"] = "handleClient",
      }
      r.handleRequest = function(self, request) -- YEILDS
        local fc = "AsyncHandler/handleRequest"
        self, request, sig = pm({self, request}, {"asynchandler", "asyncobject"})
        self:log{"function", fc, cc, request, sig}
        
        -- The meat of async: request according to AsyncObject properties
        local requestType = pt(request.requestType, "!not!nil", "request passed to " .. fc .. " in " .. cc .. " .requestType does not exist (=nil)")
        assert(self.typeMapping[requestType], "request passed to " .. fc .. " in " .. cc .. " .requestType is not a valid type (not contained within mapping)", 2)
        local index = pt(request.index, "!not!nil", "request passed to " .. fc .. " in " .. cc .. " .index does not exist (=nil)")
        local callback = request.callback
        assert(type(callback) == "nil" or type(callback) == "function", "request passed to " .. fc .. " in " .. cc .. " .callback is not type nil or function", 2)
        
        pt(self[self.typeMapping[requestType]], "function", "requestType passed to " .. fc .. " in " .. cc .. " is not contained within self")
        self[self.typeMapping[requestType]](request, index, callback)
        
        return request -- Helpful
      end
      r.handleRequests = function(self) -- YEILDS
        local fc = "AsyncHandler/handleRequests"
        self, sig = pt(self, "asynchandler", "self passed to " .. fc .. " in " .. cc .. " not type asynchandler")
        self:log{"function", fc, cc, sig}
        for k, v in pairs(self.asyncRequests) do
          self:handleRequest(self.asyncRequests[k])
        end
      end
      r.__call = function(self) -- Calls self:handleRequests
        local fc = "AsyncHandler/__default/__call"
        self, sig = pt(self, "asynchandler", "self passed to " .. fc .. " in " .. cc .. " not type asynchandler")
        self:log{"function", fc, cc, sig}
        self:handleRequests(self)
      end
      return r
    end,
  }
  CommunicationChannel = CommunicationObject:New{ -- (Parent:) represents a means of communication
    __type = "communicationchannel",
    __name = "[communicationchannel inherited]",
    __default = function(self) -- Children of this class call this as it is in their metatable
      local fc = "CommunicationChannel/__default"
      self = pt(self, "table", "self passed to " .. fc .. " in " .. cc .. " not type table")
      local r = {}

      r.__type = "communicationchannel inherited"
      r.__name = "[communication channel :( inherited]"
      
      -- Must be overridden
      r.get = nil
      r.set = nil
      r.cash = {} -- OVERRIDE
      r.rawGet = nil
      r.rawSet = nil

      r.request = function(self, callback)
        fc = "CommunicationChannel/__default/request"
        self = pm({self, callback}, {"communicationchannel", "nil!or!function"})
      end
      
      r.prefixIndex = r.prefixIndex or "" -- OVERRIDE as per below constructor
      r.NewIndexed = function(self, index)
        fc = "CommunicationChannel/__default/NewIndexed"
        self, index, sig = pm({self, index}, {"communicationchannel", "string"}, {"self", "index"}, {cc, fc})
        self:log{"function", fc, cc, sig}
        return self:New{prefixIndex = self.prefixIndex + index}
      end

      return r
    end,
  }
  Communication = CommunicationObject:New{ -- (Parent:) defines a server's communication (state)
    __type = "communication",
    __name = "[communication]",
    __default = function(self)
      local fc = "Communication/__default"
      self = pt(self, "communication", "self passed to " .. fc .. " in " .. cc .. " not type communication")
      local r = {}

      r.__type = "communication inherited"
      r.__name = "[communication :( inherited]" -- Add default name to all children
      r.channels = {}
      r.addChannel = function(self, channel, index)
        local fc = "Communication/__default/addChannel"
        self, channel = pm({self, channel}, {"communication", "communicationchannel"}, {"self", "channel"}, {fc, cc})
        self:log{"function", fc, cc}
        if index == nil then
          table.insert(self.channels, channel)
        else
          self.channels[index] = channel
        end
        return channel
      end
      return r
    end,
  }
  CashedData = CommunicationObject:New{ 
    __type = "casheddata",
    __name = "[cashed data :( inherited]",
    data = nil, -- Overrided
    __default = function(self)
      local fc = "CashedData/__default"
      self = pt(self, "casheddata", "self passed to " .. fc .. " in " .. cc .. " not type communication")
      local r = {}

      r.instinated = os.time()

      r.synced = nil -- Override as per t/f
      r.data = nil -- OVERRIDE PLEASE {}
      r.past = {}
      r.recorded = {} -- Used for extra recorded actions in CASH OBJECT
      r.updateData = function(self, data, raw)
        fc = "CashedData/__default/updateData"
        self, data, raw, sig = pm({self, data, raw or false}, {"casheddata", "!not!nil", "boolean"}, {"self", "data", "raw"}, {cc, fc})
        self:log{"function", fc, cc, sig}
        table.insert(self.past, {data = data, timeOfUpdate = os.time(), __type = "casheddata/past"})
        self.data = data
      return self
      end
      r.recordAccess = function(self, ...)
        fc = "CashedData/__default/recordAccess"
        self = pt(self, "casheddata", "self passed to " .. fc .. " in " .. cc .. " is not type casheddata")
        table.insert(self.recorded, {time = os.time(), extra = {...}, __type = "casheddata/recorded"})
      end
      return r
    end
  }

end

--------------------------------------------------------------

cc = "object implementation"

RUNTIME = RuntimeHandler:New{ -- IMPORTANT RuntimeHandler ABSOLUTE GLOBAL REFERENCES EXIST > 
    __type = "RuntimeHandler",
    __name = "[Runtime Handler]",
    threads = false,
    queuedForInspection = {},
    queueForInspection = function(self, objRef)
      fc = "RUNTIME/queueForInspection"
      self, sig = pt(self, "RuntimeHandler", "self passed to " .. fc .. " in " .. cc .. " not type RuntimeHandler")
      self:log{"function", fc, cc, sig}
      table.insert(self.queuedForInspection, objRef)
    end,
    inspectQueued = function(self)
      fc = "RUNTIME/inspectQueued"
      self, sig = pt(self, "RuntimeHandler", "self passed to " .. fc .. " in " .. cc .. " not type RuntimeHandler")
      self:log{"function", fc, cc, sig}
      for k, v in pairs(self.queuedForInspection) do
        if v.instinate then self:subscribeStart(v) end
        if v.terminate then self:subscribeTerminate(v) end
        if v.step then self:subscribeStep(v) end
      end
      return
    end,    
  }

cashObj = CashedData:New{
  __type = "cashobj",
  __name = "[cashObj :( inherited]",
}

cc = "object implementation/communication"
-- Represents the data store local to this game (and script, see 'dataStoreName' definition in first cc section)
localDataStore = CommunicationChannel:New{ -- Will yeild as per usage
  __type = "CommunicationChannel",
  __name = "[LocalDataStore]",
  reference = StandardDataStore,
  cash = {},
  record = {}, -- Record for debugging of CHANNEL
  types = {
    set = Enum.DataStoreRequestType.SetIncrementAsync,
    get = Enum.DataStoreRequestType.GetAsync,
    update = Enum.DataStoreRequestType.UpdateAsync,
    GetSorted = Enum.DataStoreRequestType.GetSortedAsync,
    SetIncrementSorted = Enum.DataStoreRequestType.SetIncrementSortedAsync
  },
  yeildRequest = function(self, type)
    fc = "localDataStore/yeildRequest"
    self, type = pm({self, type}, {"communicationchannel"}, {"self", "type"}, {fc, cc})
    self:log{"function", fc, cc}
    local getBudget = function() 
      local suc, result = pcall(function () return DataStoreService:GetRequestBudgetForRequestType(type) end)
      assert(suc, "Manually caught error in " .. cc .. " in " .. fc .. " while attempting to read budget (most likely bad type param): " .. result)  
    end
    -- Manage how many requests go through data store service
    if getBudget() >= 1 then
      return
    end
    local n = 0 -- For debugging purposes and just out of interest
    while getBudget() < 5 do -- Yeild until enough requests can be received
      wait(5) -- Yeild the thread until budget
      n += 1
      self:log("function", fc, cc, "yeilding", n)
    end
  end,
  recordRequest = function(self, type, index, ...) -- Record **RAW** requests (to DS) yeilds
    fc = "localDataStore/recordRequest"
    local args -- Ensures clarity over variable scopes
    self, type, index, args = pm({self, type, index, {...} or {}}, {"communicationchannel"}, {"self", "type", "index", "args"}, {cc, fc}, {...})
    self:log{"function", fc, cc}
    table.insert(self.record, {type = type, time = os.time(), index = index, extra = args})
    self:yeildRequest(type)
    if type == self.types.set then

    elseif type == self.types.get then
      self.cash[index]:recordAccess()
    end
  end,
  rawGet = function (self, index, callback) -- Retrieves value at index in database ONLY
    fc = "localDataStore/rawGet"
    self, index, callback, sig = pm({self, index, callback or function () end}, {"communicationchannel", "string", "function"}, {"self", "index", "callback"}, {cc, fc})
    self:log{"function", fc, cc, sig}
    local success, value, errorMessage = pcall(function ()
      local success, value, errorMessage = self.reference:GetAsync(index)
      self:recordRequest(self.types.get, index, {success = success, value = value, errorMessage = errorMessage})
      return success, value, errorMessage
    end)
    if callback then
      callback(success, value, errorMessage)
    end
    return success, value, errorMessage
  end,
  rawSet = function (self, index, value, callback) -- Sets value in raw database and callbacks ONLY
    fc = "localDataStore/rawSet"
    self, index, value, callback, sig = pm({self, index, value, callback or function () end}, {"communicationchannel", "string", "!not!nil", "function"}, {"self", "index", "value", "callback"}, {cc, fc})
    self:log{"function", fc, cc, sig}
    local success, errorMessage = pcall(function ()
      local success, errorMessage = self.reference:SetAsync(index, value)
      self:recordRequest(self.types.set, index, {success = success, errorMessage = errorMessage, value = value})
      return success, errorMessage
    end)
    if callback then
      callback(success, errorMessage, value)
    end
    return success, errorMessage, value
  end,
  get = function(self, index, callback) -- Retrieves value from cash OR raw > 
    fc = "localDataStore/get"
    self, index, callback = pm(pm({self, index, callback or function () end}, {"communicationchannel", "string", "function"}, {"self", "index", "callback"}, {cc, fc}))
    self:log{"function", fc, cc}
    if self.cash[index] then
      local data = self.cash[index].data
      if callback then
        callback("CASHED", data)
      end
      return "CASHED", data
    else
      local success, value, errorMessage = self:rawGet(index, callback)
      return success, value, errorMessage
    end
  end,
  set = function(self, index, value, callback) -- Sets value in cash OR raw AND creates new cash
    fc = "localDataStore/set"
    self, index, callback = pm(pm({self, index, value, callback or function () end}, {"communicationchannel", "string", "!not!nil", "function"}, {"self", "index", "value", "callback"}, {cc, fc}))
    self:log{"function", fc, cc}
    if self.cash[index] then
      self.cash[index]:updateData(value, false) -- Updates data to not synced
      if callback then callback("CASHED", value) end
    else
      local success, errorMessage = self:rawSet(index, value, callback)
      if success then self.cash[index] = (cashObj:New{}):updateData(value, true) end -- Store retrieved data in cash (if successfully retrieved)
      return success, value, errorMessage
    end
  end,
}

MultiServerCommunication = Communication:New{ -- Allows regulated communication between INFINITE other servers in same game
  __type = "MultiServerCommunication",
  __name = "[MultiServerCommunication]",
  channels = {
    L_DS = localDataStore, -- Local _ Data Store
    L_DSPlayer = nil,
    L_DSServerManagement = nil,
    C_BossID = nil, -- Will add dynamic reference to channels.L_DS:NewIndexed("Player/ServerManagement/BOSS SERVER ID")
    C_ToManyBosses = nil, -- Cross server _ ...
    C_Index = nil,
    C_FinalCache = nil,
  },
  __default = function(self) error("DO NOT ATTEMPT to use MultiServerCommunication or other instinated objects as parents", 1) end,
  instinate = function(self)
    fc = "MultiServerCommunication/instinate"
    self, sig = pm({self}, {"MultiServerCommunication"}, {"self"}, {fc, cc})
    self.channels.L_DSPlayer = self.channels.L_DS:NewIndexed("Player/") -- Is really unnecessary to have this folder but hey why not?
    self.channels.L_DSServerManagement = self.channels.L_DSPlayer:NewIndexed("ServerManagement/")
    self.channels.C_BossID = self.channels.L_DSServerManagement:NewIndexed("BossID")
  end,

}

--------------------------------------------------------------

cc = ""

a = Object:New{a = true}
b = a:New{b = true}
c = b:New{c = true}
d = c:New{d = true}

printT(a)

--------------------------------------------------------------

cc = ""

--------------------------------------------------------------
cc = ""

--------------------------------------------------------------
cc = ""

--------------------------------------------------------------

cc = "runtime section" -- 'Activates' and sets up indefinite running of program with high level objects
-- And when I say high, I mean *HIGH* objects, like they have been getting at the good stuff for a while now and are really gone good

--[[
print(RuntimeHandler)
print(RUNTIME)
print(RUNTIME.instinateStart)

if RUNTIME.inspectQueued then RUNTIME:inspectQueued() end -- Subscribes objects by inspecting queued references (typically added by Server.New)
--RUNTIME:instinateStart()

terminate = false
game:BindToClose(function () terminate = true end) -- Sets terminate variable to true to indicate to program that server is shutting down :(

while not terminate do
  RUNTIME()
  wait(1) -- Gives time for program to rest
end

--]]

--------------------------------------------------------------
--- Last section as runtime section by design stops all code below being executed (until terminated)


-- WAY to many lines for a normal person
-- EOF