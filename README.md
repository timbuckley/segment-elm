# Segment-elm
Elm client for segment.io which caches the events and sends them in batch.

Add Segment model to your model. Provide key, app name and optionally tick (=time between api calls in millis).
Default is 500.

    let
        segmentModel =
            Segment.defaultModel "segmentKey" "applicationName"
    in
    { segmentModel | tick = 100 }


Create some event and send it to the update function where it gets handled:

    let
        segmentMsg =
            let
                properties =
                    [ ( "title", Json.Encode.string "Welcome | Initech" )
                    , ( "url", Json.Encode.string "http://www.initech.com" )
                    ]
            in
            Segment.createIdentifiedPageEventMsg "Home" properties
    in
    update (HandleSegmentMsg segmentMsg) model


After first event is added, timer starts to tick.
Every tick - events are send. Identified events get send only if Identify event was provided. Anonymous events will be send every time.
You can force sending cached events with SendApiBatch msg.

    HandleSegmentMsg segmentMsg ->
        let
            ( updatedSegmentModel, segmentCmd ) =
                Segment.update segmentMsg model.segmentModel
        in
        ( { model | segmentModel = updatedSegmentModel }, Cmd.map HandleSegmentMsg segmentCmd )

