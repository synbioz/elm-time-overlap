module Tests exposing (..)

import Test exposing (..)
import Expect
import Date exposing (Date)
import Date.Extra.Create exposing (dateFromFields)
import Time.Overlap


unOverlapTests : Test
unOverlapTests =
    let
        makeEvent : Date -> Date -> { start : Date, end : Date }
        makeEvent start end =
            { start = start
            , end = end
            }

        extractStartEnd event =
            ( (.start event |> Date.toTime), (.end event |> Date.toTime) )
    in
        describe "unOverlap"
            [ test "should produce 3 lines for three same events" <|
                \() ->
                    let
                        baseEvent =
                            makeEvent (dateFromFields 2016 Date.Jun 26 12 0 0 0) (dateFromFields 2016 Date.Jun 28 12 0 0 0)

                        events =
                            [ baseEvent, baseEvent, baseEvent ]

                        unoverlaped =
                            Time.Overlap.unOverlap extractStartEnd events
                    in
                        Expect.equal (List.length unoverlaped) 3
            , test "should produce 2 lines if one event overlap" <|
                \() ->
                    let
                        events =
                            [ makeEvent (dateFromFields 2016 Date.Jun 26 12 0 0 0) (dateFromFields 2016 Date.Jun 28 12 0 0 0)
                            , makeEvent (dateFromFields 2016 Date.Jun 29 12 0 0 0) (dateFromFields 2016 Date.Jun 31 12 0 0 0)
                            , makeEvent (dateFromFields 2016 Date.Jun 27 12 0 0 0) (dateFromFields 2016 Date.Jun 30 12 0 0 0)
                            ]

                        unoverlaped =
                            Time.Overlap.unOverlap extractStartEnd events
                    in
                        Expect.equal (List.length unoverlaped) 2
            ]


all : Test
all =
    describe "Sample Test Suite"
        [ unOverlapTests
        ]
