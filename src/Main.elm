port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Dict
import FragmentParser as Fp
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Url
import Url.Parser as P exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Q



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


type alias Query =
    String


route : Parser (Route -> a) a
route =
    P.oneOf
        [ P.map Home P.top
        , P.map SignIn (P.s "signin" </> P.fragment identity)
        ]


type Route
    = Home
    | SignIn (Maybe String)
    | NotFound


toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound <| P.parse route url


view : Model -> Browser.Document Msg
view { url } =
    case toRoute url of
        Home ->
            { title = "ホーム"
            , body =
                [ a
                    [ class "pure-button pure-button-active"
                    , href "https://zoma-test.auth.ap-northeast-1.amazoncognito.com/login?response_type=token&client_id=3ki2rs0t3rfo4m940q2uc3oscm&redirect_uri=https://elm-cognito.netlify.com/signin"
                    ]
                    [ text "ログイン" ]
                ]
            }

        SignIn fragmentMaybe ->
            { title = "サインイン"
            , body =
                [ div []
                    [ p [] [ text "サインイン中..." ]
                    , p [] [ text <| Maybe.withDefault "" (Maybe.andThen Fp.run fragmentMaybe |> Maybe.andThen (Dict.get "access_token")) ]
                    ]
                ]
            }

        NotFound ->
            { title = "NotFound"
            , body =
                [ text "おしゃれな NotFound"
                , a [ href "/home" ] [ text "ホーム画面へ" ]
                ]
            }
