module PhotoGroove exposing (main)

import Browser
import Html exposing (button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
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


type alias Photo =
    { url : String }


type ThumbnailSize
    = Small
    | Medium
    | Large


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }



-- Update


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error String)


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

        GotPhotos result ->
            getPhotos model result


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
                |> Tuple.pair model

        Loaded [] _ ->
            ( model, Cmd.none )

        Loading ->
            ( model, Cmd.none )

        Errored _ ->
            ( model, Cmd.none )


getPhotos : Model -> Result Http.Error String -> ( Model, Cmd Msg )
getPhotos model result =
    case result of
        Ok responseStr ->
            case String.split "," responseStr of
                -- same as `const { firstUrl, ...rest } = urls` in js where `rest == _`
                (firstUrl :: _) as urls ->
                    let
                        photos =
                            List.map (\url -> { url = url }) urls
                    in
                    ( { model | status = Loaded photos firstUrl }, Cmd.none )

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        Err httpError ->
            ( { model | status = Errored "Server error!" }, Cmd.none )



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
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []
