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


dictUpdate : { key : String, itemUpdated : a, maybeDict : Maybe (Dict.Dict String a) } -> Maybe (Dict.Dict String a)
dictUpdate { key, itemUpdated, maybeDict } =
    --TODO TEST
    Maybe.map (\d -> Dict.insert key itemUpdated d |> Just) maybeDict
        |> Maybe.withDefault maybeDict


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
    = OnClickMethod { resourcekey : String, methodKey : String }
    | OnClickAction { resourcekey : String, methodKey : String, actionKey : String }
    | OnInputTextAreaApiRoutesFileConfiguration String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnClickMethod { resourcekey, methodKey } ->
            case model.blueprint of
                Ok blueprint ->
                    case Dict.get resourcekey (Maybe.withDefault Dict.empty blueprint.resources) of
                        Just resource ->
                            case Dict.get methodKey (Maybe.withDefault Dict.empty resource.methods) of
                                Just method ->
                                    let
                                        methodsUpdated =
                                            dictUpdate
                                                { key = methodKey
                                                , itemUpdated = { method | isOpened = method.isOpened |> not }
                                                , maybeDict = resource.methods
                                                }

                                        resourcesUpdated =
                                            dictUpdate
                                                { key = resourcekey
                                                , itemUpdated =
                                                    { resource
                                                        | methods = methodsUpdated
                                                    }
                                                , maybeDict = blueprint.resources
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
                    case Dict.get resourcekey (Maybe.withDefault Dict.empty blueprint.resources) of
                        Just resource ->
                            case Dict.get methodKey (Maybe.withDefault Dict.empty resource.methods) of
                                Just vmMethod ->
                                    case Dict.get actionKey (Maybe.withDefault Dict.empty vmMethod.method.actions) of
                                        Just vmAction ->
                                            let
                                                actionsUpdated =
                                                    dictUpdate
                                                        { key = actionKey
                                                        , itemUpdated = { vmAction | isOpened = vmAction.isOpened |> not }
                                                        , maybeDict = vmMethod.method.actions
                                                        }

                                                method =
                                                    vmMethod.method

                                                methodUpdated =
                                                    { method | actions = actionsUpdated }

                                                methodsUpdated =
                                                    dictUpdate
                                                        { key = methodKey
                                                        , itemUpdated = { vmMethod | method = methodUpdated }
                                                        , maybeDict = resource.methods
                                                        }

                                                resourcesUpdated =
                                                    dictUpdate
                                                        { key = resourcekey
                                                        , itemUpdated =
                                                            { resource
                                                                | methods = methodsUpdated
                                                            }
                                                        , maybeDict = blueprint.resources
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
                        [ td [ class "integration" ] [ text <| Maybe.withDefault "Propriedade não informada." vmAction.action.integration ]
                        , td [ class "proxyIntegration" ]
                            [ text <|
                                Maybe.withDefault "Propriedade não informada." <|
                                    Maybe.map
                                        (\proxyIntegration ->
                                            if proxyIntegration then
                                                "True"

                                            else
                                                "False"
                                        )
                                        vmAction.action.proxyIntegration
                            ]
                        , td [ class "vpcLink" ]
                            [ text <| Maybe.withDefault "Propriedade não informada." vmAction.action.vpcLink
                            ]
                        , td [ class "authorization" ]
                            [ text <| Maybe.withDefault "Propriedade não informada." vmAction.action.authorization
                            ]
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

        justVmMethodActions : Dict.Dict String ViewModelAction -> Html Msg
        justVmMethodActions actions =
            Dict.values actions
                |> List.map (viewAction { resourcekey = resourceKey, methodKey = vmMethod.method.path })
                |> div [ class "list-group mt-3" ]

        nothingVmMethodActions : Html Msg
        nothingVmMethodActions =
            p [ class "alert", class "alert-warning" ] [ text "A propriedade actions não informada." ]
    in
    div []
        [ button [ onClick <| OnClickMethod { resourcekey = resourceKey, methodKey = vmMethod.method.path }, class "list-group-item list-group-item-action" ]
            [ text <| vmMethod.method.path ++ "/" ++ mountPathUrlUsingQueryParams vmMethod.method.queryParams
            , if vmMethod.isOpened then
                i [ class "float-right fas", class iconClass ] []

              else
                i [ class "float-right fas", class iconClass ] []
            ]
        , if vmMethod.isOpened then
            Maybe.withDefault (p [ class "alert", class "alert-warning" ] [ text "A propriedade cors não informada." ]) <| Maybe.map viewCors vmMethod.method.cors

          else
            text ""
        , if vmMethod.isOpened then
            Maybe.withDefault nothingVmMethodActions <| Maybe.map justVmMethodActions vmMethod.method.actions

          else
            text ""
        ]


viewResource : Resource -> Html Msg
viewResource resource =
    let
        justMethods : String -> Dict.Dict String ViewModelMethod -> Html Msg
        justMethods resourceName methods =
            Dict.values methods
                |> List.map (viewMethod resourceName)
                |> div [ class "list-group col-12" ]

        nothingMethods : Html Msg
        nothingMethods =
            p [ class "alert", class "alert-warning" ] [ text "A propriedade methods não informada." ]
    in
    div [ class "row col-12" ]
        [ h3 [] [ text resource.name ]
        , viewResourceFlask resource.resourceFlask
        , Maybe.withDefault nothingMethods <| Maybe.map (justMethods resource.name) resource.methods
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
        resourcesHtml =
            case blueprint.resources of
                Just resources ->
                    Dict.values resources
                        |> List.map viewResource
                        |> div [ class "list-group" ]

                Nothing ->
                    p [ class "alert alert-warning resources" ] [ text "Resources não informados." ]
    in
    div [ class "col" ]
        [ h2 [] [ span [] [ text "Blueprint: " ], text <| Maybe.withDefault "Propriedade não informada." blueprint.name ]
        , p [] [ span [] [ text "Url Prefix: " ], text <| Maybe.withDefault "Propriedade não informada." blueprint.url_prefix ]
        , resourcesHtml
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
    p [ class "text-center" ] [ text "Bernardo Gomes -  Version 0.2" ]


viewResourceFlask : Maybe ResourceFlask -> Html Msg
viewResourceFlask maybeResourceFlask =
    let
        justResourceFlask : ResourceFlask -> Html Msg
        justResourceFlask resourceFlask =
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
                        [ td [ class "resourceModule" ] [ text <| Maybe.withDefault "Propriedade não informada." resourceFlask.resourceModule ]
                        , td [ class "resourceClass" ] [ text <| Maybe.withDefault "Propriedade não informada." resourceFlask.resourceClass ]
                        , td [ class "strictSlashes" ]
                            [ text <|
                                Maybe.withDefault "Propriedade não informada." <|
                                    Maybe.map
                                        (\strictSlashes ->
                                            if strictSlashes then
                                                "Yes"

                                            else
                                                "No"
                                        )
                                        resourceFlask.strictSlashes
                            ]
                        ]
                    ]
                ]

        nothingResourceFlask : Html Msg
        nothingResourceFlask =
            p [ class "alert", class "alert-warning" ] [ text "A propriedade flask não foi informada." ]
    in
    Maybe.withDefault nothingResourceFlask <| Maybe.map justResourceFlask maybeResourceFlask


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
                [ td [ class "enable" ]
                    [ text <|
                        Maybe.withDefault "Propriedade não informada." <|
                            Maybe.map
                                (\enable ->
                                    if enable then
                                        "Yes"

                                    else
                                        "No"
                                )
                                cors.enable
                    ]
                , td [ class "removeDefaultResponseTemplates" ]
                    [ text <|
                        Maybe.withDefault "Propriedade não informada." <|
                            Maybe.map
                                (\removeDefaultResponseTemplates ->
                                    if removeDefaultResponseTemplates then
                                        "Yes"

                                    else
                                        "No"
                                )
                                cors.removeDefaultResponseTemplates
                    ]
                , td [ class "allowHeaders" ]
                    [ text <|
                        Maybe.withDefault "Propriedade não informada." <|
                            Maybe.map (\allowHeaders -> String.join ", " allowHeaders) cors.allowHeaders
                    ]
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
