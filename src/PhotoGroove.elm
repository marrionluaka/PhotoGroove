module PhotoGroove exposing (main)

import Browser
import Html exposing (button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (checked, class, classList, id, name, src, title, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
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
    }


type ThumbnailSize
    = Small
    | Medium
    | Large


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        ClickedSurpriseMe ->
            randomPhotoPicker model

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )

        GotPhotos (Ok photos) ->
            getPhotos model photos

        GotPhotos (Err _) ->
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
                viewLoaded photos selectedUrl model.chosenSize

            Loading ->
                []

            Errored errorMessage ->
                [ text <| "Error: " ++ errorMessage ]


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html.Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , viewSurpriseMeBtn
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser chosenSize) [ Small, Medium, Large ])
    , div [ id "thumbnails", class <| sizeToString chosenSize ] (List.map (viewThumbnail selectedUrl) photos)
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
