module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (button, div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }



-- Data


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Medium
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos



-- Update


type Msg
    = ClickedPhoto String
    | ClickedSurpriseMe


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPhoto url ->
            { model | selectedUrl = url }

        ClickedSurpriseMe ->
            { model | selectedUrl = "2.jpeg" }



-- View


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


view : Model -> Html.Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , viewSurpriseMeBtn
        , div [ id "thumbnails" ] (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


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
