port module PhotoGroove exposing (main)

import Browser
import Html exposing (Attribute, button, div, h1, h3, img, input, label, node, text)
import Html.Attributes as Attr exposing (checked, class, classList, id, name, src, title, type_)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> subscriptions
        }


subscriptions : Sub msg
subscriptions =
    Sub.none



-- Data


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


type ThumbnailSize
    = Small
    | Medium
    | Large


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


port setFilters : FilterOptions -> Cmd msg


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Int }
    }


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    , hue = 5
    , ripple = 5
    , noise = 5
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (list photoDecoder)
        }



-- Update


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }

        ClickedSurpriseMe ->
            randomPhotoPicker model

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        GotPhotos (Ok photos) ->
            getPhotos model photos

        GotPhotos (Err _) ->
            ( model, Cmd.none )

        SlidHue hue ->
            ( { model | hue = hue }, Cmd.none )

        SlidRipple ripple ->
            ( { model | ripple = ripple }, Cmd.none )

        SlidNoise noise ->
            ( { model | noise = noise }, Cmd.none )


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded photos selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = model.hue }
                    , { name = "Ripple", amount = model.ripple }
                    , { name = "Noise", amount = model.noise }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model, setFilters { url = url, filters = filters } )

        Loading ->
            ( model, Cmd.none )

        Errored _ ->
            ( model, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored _ ->
            status


randomPhotoPicker : Model -> ( Model, Cmd Msg )
randomPhotoPicker model =
    case model.status of
        Loaded (firstPhoto :: otherPhotos) _ ->
            Random.uniform firstPhoto otherPhotos
                |> Random.generate GotRandomPhoto
                -- ^Once the photo selected, it'll trigger the GotRandomPhoto msg which will do the selection for you.
                -- The Elm runtime will handle the side effects, you just need to focus on the pure aspect of the code.
                |> Tuple.pair model

        Loaded [] _ ->
            ( model, Cmd.none )

        Loading ->
            ( model, Cmd.none )

        Errored _ ->
            ( model, Cmd.none )


getPhotos : Model -> List Photo -> ( Model, Cmd Msg )
getPhotos model photos =
    case photos of
        -- `(firstUrl :: _) as urls` is the same as `const { firstUrl, ...rest } = urls` in js where `rest == _`
        first :: rest ->
            ( { model | status = Loaded photos first.url }, Cmd.none )

        [] ->
            ( { model | status = Errored "0 photos found" }, Cmd.none )



-- View


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


view : Model -> Html.Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model

            Loading ->
                []

            Errored errorMessage ->
                [ text <| "Error: " ++ errorMessage ]


viewLoaded : List Photo -> String -> Model -> List (Html.Html Msg)
viewLoaded photos selectedUrl model =
    [ h1 [] [ text "Photo Groove" ]
    , viewSurpriseMeBtn
    , div [ class "filters" ]
        [ viewFilter SlidHue "Hue" model.hue
        , viewFilter SlidRipple "Ripple" model.ripple
        , viewFilter SlidNoise "Noise" model.noise
        ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
    , div [ id "thumbnails", class <| sizeToString model.chosenSize ] (List.map (viewThumbnail selectedUrl) photos)
    , img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []
    ]


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html.Html Msg
viewSizeChooser currentSize size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick (ClickedSize size)
            , checked (currentSize == size)
            ]
            []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


viewSurpriseMeBtn : Html.Html Msg
viewSurpriseMeBtn =
    button
        [ onClick ClickedSurpriseMe
        ]
        [ text "Surprise Me!"
        ]


viewThumbnail : String -> Photo -> Html.Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


viewFilter : (Int -> Msg) -> String -> Int -> Html.Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


rangeSlider : List (Attribute msg) -> List (Html.Html msg) -> Html.Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    at [ "detail", "userSlidTo" ] int
        |> Json.Decode.map toMsg
        |> on "slide"
