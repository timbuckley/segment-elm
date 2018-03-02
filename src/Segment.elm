module Segment
    exposing
        ( EventData
        , IdentifyData
        , createIdentifyCmd
        , createPageEventCmd
        , createTrackEventCmd
        )

{-| Library for sending events to segment.io


# Available commands


## Identify

@docs IdentifyData
@docs createIdentifyCmd


## Events

@docs EventData
@docs createPageEventCmd
@docs createTrackEventCmd

-}

import Base64
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Task
import Time


{-|

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
    createIdentifyCmd eventData

-}
createIdentifyCmd : IdentifyData msg -> Cmd msg
createIdentifyCmd { msg, key, applicationName, traits, userId } =
    let
        eventBody =
            [ ( "type", Encode.string "identify" )
            , ( "userId", Encode.string userId )
            , ( "traits", Encode.object traits )
            ]
    in
    createCallApiCmd msg key applicationName eventBody


{-|

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
    createPageEventCmd eventData

-}
createPageEventCmd : EventData msg -> Cmd msg
createPageEventCmd eventData =
    createEventCmd eventData "page" "name"


{-|

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
    createTrackEventCmd eventData

-}
createTrackEventCmd : EventData msg -> Cmd msg
createTrackEventCmd eventData =
    createEventCmd eventData "track" "event"


createEventCmd : EventData msg -> String -> String -> Cmd msg
createEventCmd { msg, key, applicationName, name, properties, userId, anonymous } type_ descriptor =
    let
        eventBody =
            let
                identificationFieldName =
                    if anonymous then
                        "anonymousId"
                    else
                        "userId"
            in
            [ ( "type", Encode.string type_ )
            , ( identificationFieldName, Encode.string userId )
            , ( descriptor, Encode.string name )
            , ( "properties", Encode.object properties )
            ]
    in
    createCallApiCmd msg key applicationName eventBody


{-| -}
type alias IdentifyData msg =
    { msg : WebData Decode.Value -> msg
    , key : String
    , applicationName : String
    , traits : List ( String, Encode.Value )
    , userId : String
    }


{-| -}
type alias EventData msg =
    { msg : WebData Decode.Value -> msg
    , key : String
    , applicationName : String
    , name : String
    , properties : List ( String, Encode.Value )
    , userId : String
    , anonymous : Bool
    }


createCallApiCmd : (WebData Decode.Value -> msg) -> String -> String -> List ( String, Encode.Value ) -> Cmd msg
createCallApiCmd msg key applicationName eventFields =
    let
        type_ =
            case List.head (List.filter (\( key, value ) -> key == "type") eventFields) of
                Nothing ->
                    ""

                Just ( key, value ) ->
                    value

        request =
            let
                body =
                    let
                        context =
                            [ ( "context"
                              , Encode.object
                                    [ ( "app", Encode.string applicationName )
                                    , ( "library"
                                      , Encode.object
                                            [ ( "name", Encode.string contextName )
                                            , ( "version", Encode.string contextVersion )
                                            ]
                                      )
                                    ]
                              )
                            ]
                    in
                    Encode.object (eventFields ++ context)
            in
            Http.request
                { method = "POST"
                , headers = [ Http.header "Authorization" ("Basic " ++ Base64.encode (key ++ ":")) ]
                , url = "https://api.segment.io/v1/" ++ type_
                , body = Http.jsonBody body
                , expect = Http.expectJson Decode.value
                , timeout = Nothing
                , withCredentials = True
                }
    in
    RemoteData.sendRequest request
        |> Cmd.map msg


contextName : String
contextName =
    "segment-elm"


contextVersion : String
contextVersion =
    "2.0.0"
