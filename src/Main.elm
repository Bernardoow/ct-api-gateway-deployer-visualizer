module Main exposing (Model, Msg(..), init, main, mountPathUrlUsingQueryParams, update, view, viewAction, viewBlueprint, viewCors, viewJsonErrorReport, viewMethod, viewResource, viewResourceFlask, viewTextArea)

import Browser
import Dict
import Html exposing (Html, button, div, h1, h2, h3, i, img, p, pre, span, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes exposing (class, rows, src, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Models exposing (Blueprint, Cors, QueryParam, Resource, ResourceFlask, ViewModelAction, ViewModelMethod, apiRoutesFileConfigurationDecoder)
import TestData exposing (fileExample01Data)



---- HELPERS ----


dictUpdate : { key : String, itemUpdated : a, dict : Dict.Dict String a } -> Dict.Dict String a
dictUpdate { key, itemUpdated, dict } =
    --TODO TEST
    Dict.insert key itemUpdated dict


mountPathUrlUsingQueryParams : List QueryParam -> String
mountPathUrlUsingQueryParams queryParamsList =
    List.map (\qp -> "<" ++ qp.type_ ++ ":" ++ qp.name ++ ">") queryParamsList
        |> String.join "/"



---- MODEL ----


type alias Model =
    { blueprint : Result Decode.Error Blueprint, apiRoutesFileConfigurationRaw : String }


init : String -> ( Model, Cmd Msg )
init dataToLoad =
    ( { blueprint = Decode.decodeString apiRoutesFileConfigurationDecoder dataToLoad, apiRoutesFileConfigurationRaw = dataToLoad }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | OnClickMethod String String
    | OnClickAction { resourcekey : String, methodKey : String, actionKey : String }
    | OnInputTextAreaApiRoutesFileConfiguration String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnClickMethod resourcekey methodKey ->
            case model.blueprint of
                Ok blueprint ->
                    case Dict.get resourcekey blueprint.resources of
                        Just resource ->
                            case Dict.get methodKey resource.methods of
                                Just method ->
                                    let
                                        methodsUpdated =
                                            dictUpdate
                                                { key = methodKey
                                                , itemUpdated = { method | isOpened = method.isOpened |> not }
                                                , dict = resource.methods
                                                }

                                        resourcesUpdated =
                                            dictUpdate
                                                { key = resourcekey
                                                , itemUpdated =
                                                    { resource
                                                        | methods = methodsUpdated
                                                    }
                                                , dict = blueprint.resources
                                                }
                                    in
                                    ( { model
                                        | blueprint =
                                            Ok
                                                { blueprint
                                                    | resources = resourcesUpdated
                                                }
                                      }
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        OnClickAction { resourcekey, methodKey, actionKey } ->
            case model.blueprint of
                Ok blueprint ->
                    case Dict.get resourcekey blueprint.resources of
                        Just resource ->
                            case Dict.get methodKey resource.methods of
                                Just vmMethod ->
                                    case Dict.get actionKey vmMethod.method.actions of
                                        Just vmAction ->
                                            let
                                                actionsUpdated =
                                                    dictUpdate
                                                        { key = actionKey
                                                        , itemUpdated = { vmAction | isOpened = vmAction.isOpened |> not }
                                                        , dict = vmMethod.method.actions
                                                        }

                                                method =
                                                    vmMethod.method

                                                methodUpdated =
                                                    { method | actions = actionsUpdated }

                                                methodsUpdated =
                                                    dictUpdate
                                                        { key = methodKey
                                                        , itemUpdated = { vmMethod | method = methodUpdated }
                                                        , dict = resource.methods
                                                        }

                                                resourcesUpdated =
                                                    dictUpdate
                                                        { key = resourcekey
                                                        , itemUpdated =
                                                            { resource
                                                                | methods = methodsUpdated
                                                            }
                                                        , dict = blueprint.resources
                                                        }
                                            in
                                            ( { model
                                                | blueprint =
                                                    Ok
                                                        { blueprint
                                                            | resources = resourcesUpdated
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

                Err _ ->
                    ( model, Cmd.none )

        OnInputTextAreaApiRoutesFileConfiguration configuration ->
            ( { blueprint = Decode.decodeString apiRoutesFileConfigurationDecoder configuration, apiRoutesFileConfigurationRaw = configuration }, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


viewAction : { resourcekey : String, methodKey : String } -> ViewModelAction -> Html Msg
viewAction { resourcekey, methodKey } vmAction =
    let
        actionBadgeSpecificClass =
            if vmAction.action.type_ == "GET" then
                "badge-info"

            else if vmAction.action.type_ == "POST" then
                "badge-success"

            else if vmAction.action.type_ == "DELETE" then
                "badge-danger"

            else if vmAction.action.type_ == "PUT" then
                "badge-warning"

            else
                ""
    in
    div []
        [ button
            [ onClick <|
                OnClickAction
                    { resourcekey = resourcekey
                    , methodKey = methodKey
                    , actionKey = vmAction.action.type_
                    }
            , class "list-group-item"
            , class "list-group-item-action"
            ]
            [ span [ class "badge", class actionBadgeSpecificClass ] [ text vmAction.action.type_ ]
            , span [ class "badge badge-secondary float-right" ]
                [ text <|
                    if vmAction.isOpened then
                        "Hide Details"

                    else
                        "Show Details"
                ]
            ]
        , if vmAction.isOpened then
            table [ class "table table-bordered table-sm table table-action-info" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "integration" ]
                        , th [] [ text "proxyIntegration" ]
                        , th [] [ text "vpcLink" ]
                        , th [] [ text "authorization" ]
                        ]
                    ]
                , tbody []
                    [ tr []
                        [ td [] [ text vmAction.action.integration ]
                        , td []
                            [ text <|
                                if vmAction.action.proxyIntegration then
                                    "True"

                                else
                                    "False"
                            ]
                        , td [] [ text vmAction.action.vpcLink ]
                        , td [] [ text vmAction.action.authorization ]
                        ]
                    ]
                ]

          else
            text ""
        ]


viewMethod : String -> ViewModelMethod -> Html Msg
viewMethod resourceKey vmMethod =
    let
        iconClass =
            if vmMethod.isOpened then
                "fa-arrow-up"

            else
                "fa-arrow-down"
    in
    div []
        [ button [ onClick <| OnClickMethod resourceKey vmMethod.method.path, class "list-group-item list-group-item-action" ]
            [ text <| vmMethod.method.path ++ "/" ++ mountPathUrlUsingQueryParams vmMethod.method.queryParams
            , if vmMethod.isOpened then
                i [ class "float-right fas", class iconClass ] []

              else
                i [ class "float-right fas", class iconClass ] []
            ]
        , if vmMethod.isOpened then
            viewCors vmMethod.method.cors

          else
            text ""
        , if vmMethod.isOpened then
            Dict.values vmMethod.method.actions
                |> List.map (viewAction { resourcekey = resourceKey, methodKey = vmMethod.method.path })
                |> div [ class "list-group mt-3" ]

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
        , viewResourceFlask resource.resourceFlask
        , div [ class "list-group col-12" ] methods
        ]


viewTextArea : String -> List (Html Msg)
viewTextArea apiRoutesFileConfigurationRaw =
    [ h3 [] [ text "API Routes File Configuration" ]
    , div [ class "form-group" ]
        [ textarea [ value apiRoutesFileConfigurationRaw, rows 30, onInput OnInputTextAreaApiRoutesFileConfiguration, class "form-control" ] []
        ]
    ]


viewBlueprint : Blueprint -> Html Msg
viewBlueprint blueprint =
    let
        resources =
            Dict.values blueprint.resources
                |> List.map viewResource
    in
    div [ class "col" ]
        [ h2 [] [ span [] [ text "Blueprint: " ], text blueprint.name ]
        , p [] [ span [] [ text "Url Prefix: " ], text blueprint.url_prefix ]
        , div [ class "list-group" ] resources
        ]


viewJsonErrorReport : Decode.Error -> Html Msg
viewJsonErrorReport error =
    case error of
        Decode.Failure errorPhrase _ ->
            p [ class "alert alert-warning" ] [ text errorPhrase ]

        _ ->
            p [ class "alert alert-warning" ] [ text <| Decode.errorToString error ]


viewVersion : Html Msg
viewVersion =
    --TODO TEST
    p [ class "text-center" ] [ text "Bernardo Gomes -  Version 0.1" ]


viewResourceFlask : ResourceFlask -> Html Msg
viewResourceFlask resourceFlask =
    table [ class "table", class "table-sm", class "table-bordered" ]
        [ thead []
            [ tr []
                [ th [] [ text "Resource Module" ]
                , th [] [ text "Resource Class" ]
                , th [] [ text "Strict Slashes" ]
                ]
            ]
        , tbody []
            [ tr []
                [ td [] [ text resourceFlask.resourceModule ]
                , td [] [ text resourceFlask.resourceClass ]
                , td []
                    [ text <|
                        if resourceFlask.strictSlashes then
                            "Yes"

                        else
                            "No"
                    ]
                ]
            ]
        ]


viewCors : Cors -> Html Msg
viewCors cors =
    table [ class "table", class "table-sm", class "table-bordered", class "mt-2" ]
        [ thead []
            [ tr []
                [ th [] [ text "Enable" ]
                , th [] [ text "Remove Default Response Templates" ]
                , th [] [ text "Allow Headers" ]
                ]
            ]
        , tbody []
            [ tr []
                [ td []
                    [ text <|
                        if cors.enable then
                            "Yes"

                        else
                            "No"
                    ]
                , td []
                    [ text <|
                        if cors.removeDefaultResponseTemplates then
                            "Yes"

                        else
                            "No"
                    ]
                , td [] [ text <| String.join ", " cors.allowHeaders ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    --"TODO TEST"
    case model.blueprint of
        Ok blueprint ->
            div [ class "container-fluid" ]
                [ div [ class "row" ]
                    [ div [ class "col" ] <| viewTextArea model.apiRoutesFileConfigurationRaw
                    , viewBlueprint blueprint
                    ]
                , viewVersion
                ]

        Err error ->
            div [ class "container-fluid" ]
                [ div [ class "row" ]
                    [ div [ class "col" ] <| viewTextArea model.apiRoutesFileConfigurationRaw
                    , div [ class "col" ]
                        [ viewJsonErrorReport error ]
                    ]
                , viewVersion
                ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init fileExample01Data
        , update = update
        , subscriptions = always Sub.none
        }
