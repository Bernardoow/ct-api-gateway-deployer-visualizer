module Main exposing (Model, Msg(..), init, main, update, view, viewAction, viewMethod, viewResource)

import Browser
import Dict
import Html exposing (Html, button, div, h1, h3, img, table, tbody, td, text, textarea, tr)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Models exposing (Blueprint, Resource, ViewModelAction, ViewModelMethod, blueprintDecoder)
import TestData exposing (blueprintData)



---- MODEL ----


type alias Model =
    { blueprint : Maybe Blueprint }


init : ( Model, Cmd Msg )
init =
    ( { blueprint = Decode.decodeString blueprintDecoder blueprintData |> Result.toMaybe }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | OnClickMethod String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnClickMethod resourcekey methodKey ->
            case model.blueprint of
                Just blueprint ->
                    case Dict.get resourcekey blueprint.resources of
                        Just resource ->
                            case Dict.get methodKey resource.methods of
                                Just method ->
                                    ( { model
                                        | blueprint =
                                            Just
                                                { blueprint
                                                    | resources =
                                                        Dict.insert resourcekey
                                                            { resource
                                                                | methods =
                                                                    Dict.insert methodKey { method | isOpened = method.isOpened |> not } resource.methods
                                                            }
                                                            blueprint.resources
                                                }
                                      }
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


viewAction : ViewModelAction -> Html Msg
viewAction vmAction =
    div []
        [ button [ class "list-group-item", class "list-group-item-action" ] [ text vmAction.action.type_ ]
        , if vmAction.isOpened then
            table [ class "table" ]
                [ tbody []
                    [ tr []
                        [ td [] [ text "integration" ]
                        , td [] [ text vmAction.action.integration ]
                        ]
                    , tr []
                        [ td [] [ text "proxyIntegration" ]
                        , td []
                            [ text <|
                                if vmAction.action.proxyIntegration then
                                    "True"

                                else
                                    "False"
                            ]
                        ]
                    , tr
                        []
                        [ td [] [ text "vpcLink" ]
                        , td [] [ text vmAction.action.vpcLink ]
                        ]
                    , tr
                        []
                        [ td [] [ text "authorization" ]
                        , td [] [ text vmAction.action.authorization ]
                        ]
                    ]
                ]

          else
            text ""
        ]


viewMethod : String -> ViewModelMethod -> Html Msg
viewMethod resourceKey vmMethod =
    div []
        [ button [ onClick <| OnClickMethod resourceKey vmMethod.method.path ] [ text vmMethod.method.path ]
        , if vmMethod.isOpened then
            List.map viewAction vmMethod.method.actions
                |> div [ class "list-group" ]

          else
            text ""
        ]


viewResource : Resource -> Html Msg
viewResource resource =
    let
        methods =
            Dict.values resource.methods
                |> List.map (viewMethod resource.name)
    in
    div [ class "row col-12" ]
        [ h3 [] [ text resource.name ]
        , div [ class "list-group" ] methods
        ]


view : Model -> Html Msg
view model =
    --"TODO TEST"
    case model.blueprint of
        Just blueprint ->
            let
                resources =
                    Dict.values blueprint.resources
                        |> List.map viewResource
            in
            div [ class "container-fluid" ]
                [ div [ class "row" ]
                    [ div [ class "col" ] [ textarea [] [] ]
                    , div [ class "col" ] [ div [ class "list-group" ] resources ]
                    ]
                ]

        Nothing ->
            text "blueprint nothing"



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
