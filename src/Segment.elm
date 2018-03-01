module Segment
    exposing
        ( Model
        , Msg
        , createAnonymousPageEventMsg
        , createAnonymousTrackEventMsg
        , createIdentifiedPageEventMsg
        , createIdentifiedTrackEventMsg
        , createIdentifyEventMsg
        , defaultModel
        , update
        )

import Base64
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Task
import Time


type alias UserId =
    String


type alias Model =
    { key : String
    , userId : String
    , started : Bool
    , app : String
    , name : String
    , version : String
    , identifiedEvents : List (UserId -> Encode.Value)
    , anonymousEvents : List Encode.Value
    , eventsToBeSend : List Encode.Value
    , timeout : Int
    , timer : Int
    , lastBatchRequestState : WebData Encode.Value
    }


defaultModel : Model
defaultModel =
    { key = ""
    , userId = ""
    , started = False
    , app = ""
    , name = ""
    , version = ""
    , identifiedEvents = []
    , anonymousEvents = []
    , eventsToBeSend = []
    , timeout = 2
    , timer = 2
    , lastBatchRequestState = NotAsked
    }


type Msg
    = HandleTick
    | AddIdentifiedEvent (UserId -> Encode.Value)
    | AddAnonymousEvent Encode.Value
    | SendApiBatch
    | UpdateApiResponseState (WebData Encode.Value)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleTick ->
            let
                sleeperCmd =
                    Task.attempt (\_ -> HandleTick) (Process.sleep (Time.second * 1))

                newTimer =
                    if model.timer == 0 then
                        model.timeout
                    else
                        model.timer - 1

                sendAll =
                    not (List.isEmpty model.identifiedEvents) && not (String.isEmpty model.userId)

                eventsToBeSend =
                    let
                        eventsWhichPreviouslyFailedToBeSend =
                            model.eventsToBeSend

                        identifiedEvents =
                            List.map (\eventWithoutId -> eventWithoutId model.userId) model.identifiedEvents
                    in
                    if model.timer == 0 then
                        if sendAll then
                            eventsWhichPreviouslyFailedToBeSend ++ identifiedEvents ++ model.anonymousEvents
                        else
                            eventsWhichPreviouslyFailedToBeSend ++ model.anonymousEvents
                    else
                        model.eventsToBeSend

                ( batchApiCmdIfNeeded, anonymousEvents, identifiedEvents ) =
                    if model.timer == 0 then
                        if sendAll then
                            ( createApiBatchCmd model eventsToBeSend, [], [] )
                        else if not (List.isEmpty model.anonymousEvents) then
                            ( createApiBatchCmd model eventsToBeSend, [], model.identifiedEvents )
                        else
                            ( Cmd.none, model.anonymousEvents, model.identifiedEvents )
                    else
                        ( Cmd.none, model.anonymousEvents, model.identifiedEvents )
            in
            ( { model
                | timer = newTimer
                , eventsToBeSend = eventsToBeSend
                , anonymousEvents = anonymousEvents
                , identifiedEvents = identifiedEvents
                , started = True
              }
            , Cmd.batch [ sleeperCmd, batchApiCmdIfNeeded ]
            )

        SendApiBatch ->
            update HandleTick { model | timer = 0 }

        AddIdentifiedEvent eventWithoutUserId ->
            let
                userIdIfIdentifyEvent =
                    let
                        eventValue =
                            eventWithoutUserId ""

                        decoder =
                            let
                                userIdIfIdentify type_ =
                                    if type_ == "identify" then
                                        Decode.field "userId" Decode.string
                                    else
                                        Decode.succeed ""
                            in
                            Decode.field "type" Decode.string
                                |> Decode.andThen userIdIfIdentify
                    in
                    if String.isEmpty model.userId then
                        case Decode.decodeValue decoder eventValue of
                            Ok userId ->
                                userId

                            Err er ->
                                ""
                    else
                        model.userId

                updatedModel =
                    { model | identifiedEvents = eventWithoutUserId :: model.identifiedEvents, userId = userIdIfIdentifyEvent }
            in
            if model.started then
                ( updatedModel, Cmd.none )
            else
                update HandleTick updatedModel

        AddAnonymousEvent event ->
            let
                updatedModel =
                    { model | anonymousEvents = event :: model.anonymousEvents }
            in
            if model.started then
                ( updatedModel, Cmd.none )
            else
                update HandleTick updatedModel

        UpdateApiResponseState reqState ->
            case reqState of
                Success jsonValue ->
                    ( { model | lastBatchRequestState = reqState, eventsToBeSend = [] }, Cmd.none )

                _ ->
                    ( { model | lastBatchRequestState = reqState }, Cmd.none )


createIdentifiedPageEventMsg : String -> List ( String, Encode.Value ) -> Msg
createIdentifiedPageEventMsg name properties =
    let
        createIdentifiedPageEvent userId =
            createPageEvent name properties ( "userId", userId )
    in
    AddIdentifiedEvent createIdentifiedPageEvent


createAnonymousPageEventMsg : String -> List ( String, Encode.Value ) -> String -> Msg
createAnonymousPageEventMsg name properties anonymousId =
    let
        createAnonymousPageEvent =
            createPageEvent name properties ( "anonymousId", anonymousId )
    in
    AddAnonymousEvent createAnonymousPageEvent


createPageEvent : String -> List ( String, Encode.Value ) -> ( String, String ) -> Encode.Value
createPageEvent name properties identification =
    eventAsJsonValue "page" [ ( "name", Encode.string name ), ( "properties", Encode.object properties ) ] identification


createIdentifiedTrackEventMsg : String -> List ( String, Encode.Value ) -> Msg
createIdentifiedTrackEventMsg event properties =
    let
        createIdentifiedTrackEvent userId =
            createTrackEvent event properties ( "userId", userId )
    in
    AddIdentifiedEvent createIdentifiedTrackEvent


createAnonymousTrackEventMsg : String -> List ( String, Encode.Value ) -> String -> Msg
createAnonymousTrackEventMsg event properties anonymousId =
    let
        createAnonymousTrackEvent =
            createTrackEvent event properties ( "anonymousId", anonymousId )
    in
    AddAnonymousEvent createAnonymousTrackEvent


createTrackEvent : String -> List ( String, Encode.Value ) -> ( String, String ) -> Encode.Value
createTrackEvent event properties identification =
    eventAsJsonValue "track" [ ( "event", Encode.string event ), ( "properties", Encode.object properties ) ] identification


createIdentifyEventMsg : List ( String, Encode.Value ) -> String -> Msg
createIdentifyEventMsg traits userId =
    let
        --        dummy needs to stay there so the signature remains the same for all the events
        createIdentifyEvent dummy =
            eventAsJsonValue "identify" [ ( "traits", Encode.object traits ) ] ( "userId", userId )
    in
    AddIdentifiedEvent createIdentifyEvent


eventAsJsonValue : String -> List ( String, Encode.Value ) -> ( String, String ) -> Encode.Value
eventAsJsonValue type_ bodyPart ( field, value ) =
    Encode.object
        ([ ( "type", Encode.string type_ )
         , ( field, Encode.string value )
         ]
            ++ bodyPart
        )


createApiBatchCmd : Model -> List Encode.Value -> Cmd Msg
createApiBatchCmd model eventsToBeSend =
    let
        request =
            let
                body =
                    let
                        context =
                            [ ( "context"
                              , Encode.object
                                    [ ( "app", Encode.string model.app )
                                    , ( "library"
                                      , Encode.object
                                            [ ( "name", Encode.string model.name )
                                            , ( "version", Encode.string model.version )
                                            ]
                                      )
                                    ]
                              )
                            ]
                    in
                    Encode.object
                        ([ ( "batch", Encode.list eventsToBeSend ) ]
                            ++ context
                        )
            in
            createSegmentRequest body model.key
    in
    RemoteData.sendRequest request
        |> Cmd.map UpdateApiResponseState


createSegmentRequest : Encode.Value -> String -> Http.Request Decode.Value
createSegmentRequest bodyValue key =
    let
        headers =
            [ Http.header "Authorization" ("Basic " ++ Base64.encode (key ++ ":"))
            , Http.header "Content-Type" "application/json"
            ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "https://api.segment.io/v1/batch"
        , body = Http.jsonBody bodyValue
        , expect = Http.expectJson Decode.value
        , timeout = Nothing
        , withCredentials = False
        }
