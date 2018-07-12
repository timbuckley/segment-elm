# Segment-elm
Library to simplify calls to segment.io

### Create identify command

```elm
let
    eventData =
        let
            traits =
                [ ( "email", Json.Encode.string "pgibbons@initech.com" )
                , ( "name", Json.Encode.string "Peter Gibbons" )
                ]
        in
        { msg = UpdateIdentifyReqStateMsg
        , key = "segmentKey"
        , applicationName = "myApplicationName"
        , traits = traits
        , userId = "1234"
        }
in
Segment.createIdentifyCmd eventData
```

### Create page command
```elm
let
    eventData =
        let
            properties =
                [ ( "title", Json.Encode.string "Welcome | Initech" )
                , ( "url", Json.Encode.string "http://www.initech.com" )
                ]
        in
        { msg = UpdatePageEventReqState
        , key = "segmentKey"
        , applicationName = "myApplicationName"
        , name = "Home"
        , properties = properties
        , userId = "1234"
        , anonymous = False
        }
in
Segment.createPageEventCmd eventData
```
    
### Create track command

```elm
let
    eventData =
        let
            properties =
                [ ( "plan", Json.Encode.string "Pro Annual" )
                , ( "accountType", Json.Encode.string "Facebook" )
                ]
        in
        { msg = UpdateTrackEventReqState
        , key = "segmentKey"
        , applicationName = "myApplicationName"
        , name = "Registered"
        , properties = properties
        , userId = "someAnonymousId"
        , anonymous = True
        }
in
Segment.createTrackEventCmd eventData
```
