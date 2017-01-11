module Time.Overlap exposing (Event, unOverlap, overlap)

{-| This library provides tools for dealing with overlaping events (ie something
with a start and end time)

# Definition
@docs Event

# Usage
@docs unOverlap, overlap

-}

import Time exposing (Time)
import List exposing (any, concat)


{-| Tuple that symbolizes an event like (start, end)
-}
type alias Event =
    ( Time, Time )


{-| Return a list of list of Event with a start and an end that won't be
overlapped

**Note**: Anything can be represented as an event as long as you pass a function
that will transform your object (record, tuple...) into a 2-tuple with start
and end time.

    events = [ ("2017-01-10", "2017-01-13")
             , ("2017-01-14", "2017-01-15")
             , ("2017-01-12", "2017-01-14")
             ]

    convertDateToTime : (String, String) -> (Time, Time)

    unOverlap convertDateToTime events == [ [ ("2017-01-10", "2017-01-13")
                                            , ("2017-01-14", "2017-01-15")
                                            ]
                                          , [ ("2017-01-12", "2017-01-14") ]
                                          ]
-}
unOverlap : (a -> Event) -> List a -> List (List a)
unOverlap eventGetter events =
    makeNonoverlappedList eventGetter events []


{-| Iterate over provided events checking placing them within provided rows
-}
makeNonoverlappedList : (a -> Event) -> List a -> List (List a) -> List (List a)
makeNonoverlappedList eventGetter events rows =
    case events of
        [] ->
            rows

        hd :: tl ->
            makeNonoverlappedList eventGetter tl (fillrows eventGetter hd rows [])


fillrows : (a -> Event) -> a -> List (List a) -> List (List a) -> List (List a)
fillrows eventGetter event rows filledrows =
    case rows of
        [] ->
            [ event ] :: filledrows

        hd :: tl ->
            if not (overlapRow eventGetter event hd) then
                (event :: hd) :: concat [ tl, filledrows ]
            else
                fillrows eventGetter event tl (hd :: filledrows)


overlapRow : (a -> Event) -> a -> List a -> Bool
overlapRow eventGetter event events =
    any (\e -> overlapEvent eventGetter event e) events


{-| Check if two Events overlap or not

    overlap (1.0, 10.0) (5.0, 6.0) == True

    overlap (1.0, 2.0) (3.0, 4.0) == False
-}
overlap : Event -> Event -> Bool
overlap ( startOfA, endOfA ) ( startOfB, endOfB ) =
    (startOfA <= endOfB) && (endOfA >= startOfB)


overlapEvent : (a -> Event) -> a -> a -> Bool
overlapEvent eventGetter a b =
    overlap (eventGetter a) (eventGetter b)
