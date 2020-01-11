module TestView exposing (viewsTests)

import Dict
import Expect exposing (Expectation)
import Html exposing (div)
import Html.Attributes as HtmlAttr
import Json.Decode as Decode
import Json.Encode as Encode
import Main as Main
import Models exposing (Blueprint, Cors, Method, ResourceFlask, ViewModelAction, actionDecoder, blueprintDecoder, corsDecoder, methodDecoder, queryParamsDecoder, resourceDecoder, resourceFlaskDecoder)
import ProgramTest exposing (ProgramTest)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import TestData exposing (actionData, corsData, fileExample01Data, methodData, queryParamsData, resourceData)


startCustom : String -> ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
startCustom dataToLoad =
    ProgramTest.createElement
        { init = \_ -> Main.init dataToLoad
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.start ()


start : ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
start =
    startCustom fileExample01Data


customViewModelAction : { method : String, vpcLink : Maybe String, authorization : Maybe String, integration : Maybe String, proxyIntegration : Maybe Bool } -> ViewModelAction
customViewModelAction { method, vpcLink, authorization, integration, proxyIntegration } =
    { action =
        { type_ = method
        , integration = integration
        , proxyIntegration = proxyIntegration
        , vpcLink = vpcLink
        , authorization = authorization
        }
    , isOpened = False
    }


defaultViewModelAction : ViewModelAction
defaultViewModelAction =
    customViewModelAction { method = "GET", vpcLink = Just "vpcLink", authorization = Just "authorization1", integration = Just "integration1", proxyIntegration = Just True }


defaultParamViewAction : { resourcekey : String, methodKey : String }
defaultParamViewAction =
    { resourcekey = "String", methodKey = "String" }


defaultMethod : Method
defaultMethod =
    { path = "path"
    , cors = Just { enable = Just True, removeDefaultResponseTemplates = Just True, allowHeaders = Just [ "header1" ] }
    , queryParams = [ { name = Just "name", type_ = Just "type" } ]
    , actions =
        Just <|
            Dict.fromList
                [ ( "GET", defaultViewModelAction )
                ]
    }


defaultBlueprint : Blueprint
defaultBlueprint =
    { name = Just "Blueprint"
    , url_prefix = Just "url_prefix"
    , resources = Just Dict.empty
    }


defaultError : Decode.Error
defaultError =
    Decode.Failure "Some error happend" (Encode.int 2)


defaultResourceFlask : ResourceFlask
defaultResourceFlask =
    { resourceModule = Just "resourceModule"
    , resourceClass = Just "resourceClass"
    , strictSlashes = Just True
    }


customCors : { enable : Maybe Bool, removeDefaultResponseTemplates : Maybe Bool, allowHeaders : Maybe (List String) } -> Cors
customCors { enable, removeDefaultResponseTemplates, allowHeaders } =
    { enable = enable
    , removeDefaultResponseTemplates = removeDefaultResponseTemplates
    , allowHeaders = allowHeaders
    }


defaultCors : Cors
defaultCors =
    customCors { enable = Just True, removeDefaultResponseTemplates = Just True, allowHeaders = Just [ "A", "B", "C" ] }


viewsTests : Test
viewsTests =
    describe "Tests Views"
        [ test "it should has a text area" <|
            \_ ->
                start
                    |> ProgramTest.expectViewHas
                        [ Selector.tag "textarea" ]
        , test "it should has a div list" <|
            \_ ->
                start
                    |> ProgramTest.expectViewHas
                        [ Selector.tag "div", Selector.class "list-group" ]
        , describe "Test The whole page"
            [ test "it should has a div list" <|
                \_ ->
                    start
                        |> ProgramTest.fillInTextarea ""
                        |> ProgramTest.expectViewHas
                            [ Selector.tag "p", Selector.classes [ "alert", "alert-info" ], Selector.containing [ Selector.text "Insira as rotas na área de texto a esquerda." ] ]
            ]
        , describe "Test viewResource"
            [ test "it should has a title h3" <|
                \_ ->
                    Main.viewResource { name = "Resource 1", resourceFlask = Just defaultResourceFlask, methods = Just Dict.empty }
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "h3", Selector.containing [ Selector.text "Resource 1" ] ]
            , test "it should has a div with class list-group" <|
                \_ ->
                    Main.viewResource { name = "Resource 1", resourceFlask = Just defaultResourceFlask, methods = Just Dict.empty }
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "div", Selector.classes [ "list-group", "col-12" ] ]
            , test "it should has a table for resourceFlask" <|
                \_ ->
                    Main.viewResource { name = "Resource 1", resourceFlask = Just defaultResourceFlask, methods = Just Dict.empty }
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "table", Selector.classes [ "table", "table-sm", "table-bordered" ] ]
            , test "it should has information about nothing." <|
                \_ ->
                    Main.viewResource { name = "Resource 1", resourceFlask = Just defaultResourceFlask, methods = Nothing }
                        |> Query.fromHtml
                        |> Query.has [ Selector.all [ Selector.tag "p", Selector.classes [ "alert", "alert-warning" ], Selector.containing [ Selector.text "A propriedade methods não informada." ] ] ]
            ]
        , describe "Test viewMethod"
            [ test "it should has a margin on actions methods" <|
                \_ ->
                    Main.viewMethod "" { method = { defaultMethod | path = "endpointA" }, isOpened = True }
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "div", Selector.classes [ "list-group", "mt-3" ] ]
            , test "it should has a button with method information" <|
                \_ ->
                    Main.viewMethod "" { method = { defaultMethod | path = "endpointA" }, isOpened = False }
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "button", Selector.classes [ "list-group-item", "list-group-item-action" ], Selector.containing [ Selector.text "endpointA/<type:name>" ] ]
            , test "it should has a icon arrow down when isOpened is false" <|
                \_ ->
                    Main.viewMethod "" { method = { defaultMethod | path = "endpointA" }, isOpened = False }
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "i", Selector.classes [ "fas", "fa-arrow-down", "float-right" ], Selector.containing [ Selector.text "endpointA/" ] ]
            , test "it should has a icon arrow down when isOpened is True" <|
                \_ ->
                    Main.viewMethod "" { method = { defaultMethod | path = "endpointA" }, isOpened = True }
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "i", Selector.classes [ "fas", "fa-arrow-up", "float-right" ], Selector.containing [ Selector.text "endpointA/" ] ]
            , test "it should not show up the information about actions" <|
                \_ ->
                    Main.viewMethod "" { method = { defaultMethod | path = "endpointA" }, isOpened = False }
                        |> Query.fromHtml
                        |> Query.hasNot [ Selector.tag "div", Selector.class "list-group" ]
            , test "it should show up the information about actions" <|
                \_ ->
                    Main.viewMethod "" { method = { defaultMethod | path = "endpointA" }, isOpened = True }
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.tag "div"
                            , Selector.class "list-group"
                            , Selector.containing [ Selector.tag "button", Selector.classes [ "list-group-item", "list-group-item-action" ] ]
                            ]
            , test "it should show up the information query params missing information" <|
                \_ ->
                    Main.viewMethod "" { method = { defaultMethod | queryParams = [ { name = Nothing, type_ = Nothing } ] }, isOpened = True }
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.all
                                [ Selector.tag "button"
                                , Selector.containing [ Selector.text "Parâmetro tipo não informado" ]
                                ]
                            , Selector.all
                                [ Selector.tag "button"
                                , Selector.containing [ Selector.text "Parâmetro nome não informado." ]
                                ]
                            ]
            , test "it should show up the information about nothing properties" <|
                \_ ->
                    Main.viewMethod "" { method = { defaultMethod | cors = Nothing, actions = Nothing }, isOpened = True }
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.all
                                [ Selector.tag "p"
                                , Selector.classes [ "alert", "alert-warning" ]
                                , Selector.containing [ Selector.text "A propriedade cors não informada." ]
                                ]
                            , Selector.all
                                [ Selector.tag "p"
                                , Selector.classes [ "alert", "alert-warning" ]
                                , Selector.containing [ Selector.text "A propriedade actions não informada." ]
                                ]
                            ]
            , test "it should show up the actions " <|
                \_ ->
                    start
                        |> ProgramTest.clickButton "path"
                        |> ProgramTest.expectViewHas
                            [ Selector.tag "button", Selector.containing [ Selector.text "GET" ] ]
            , test "it should not show up the actions " <|
                \_ ->
                    start
                        |> ProgramTest.clickButton "path"
                        |> ProgramTest.clickButton "path"
                        |> ProgramTest.expectViewHasNot
                            [ Selector.tag "button", Selector.containing [ Selector.text "GET" ] ]
            ]
        , describe "Test viewAction"
            [ test "it should has a button with method information" <|
                \_ ->
                    Main.viewAction defaultParamViewAction defaultViewModelAction
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "button", Selector.containing [ Selector.text "GET" ] ]
            , test "it should has a span with text show details when isOpened is True" <|
                \_ ->
                    Main.viewAction defaultParamViewAction defaultViewModelAction
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "span", Selector.classes [ "badge", "badge-secondary", "float-right" ], Selector.containing [ Selector.text "Show Details" ] ]
            , test "it should has a span with text hide details when isOpened is False" <|
                \_ ->
                    Main.viewAction defaultParamViewAction { defaultViewModelAction | isOpened = True }
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "span", Selector.classes [ "badge", "badge-secondary", "float-right" ], Selector.containing [ Selector.text "Hide Details" ] ]
            , test "it should has a button contain a classes badge badge-info when it action is get" <|
                \_ ->
                    Main.viewAction defaultParamViewAction (customViewModelAction { method = "GET", vpcLink = Just "vpcLink", authorization = Just "authorization", integration = Just "integration1", proxyIntegration = Just True })
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "button", Selector.containing [ Selector.tag "span", Selector.classes [ "badge", "badge-info" ], Selector.text "GET" ] ]
            , test "it should has a button contain a classes badge badge-success when it action is post" <|
                \_ ->
                    Main.viewAction defaultParamViewAction (customViewModelAction { method = "POST", vpcLink = Just "vpcLink", authorization = Just "authorization", integration = Just "integration1", proxyIntegration = Just True })
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "button", Selector.containing [ Selector.tag "span", Selector.classes [ "badge", "badge-success" ], Selector.text "POST" ] ]
            , test "it should has a button contain a classes badge badge-danger when it action is delete" <|
                \_ ->
                    Main.viewAction defaultParamViewAction (customViewModelAction { method = "DELETE", vpcLink = Just "vpcLink", authorization = Just "authorization", integration = Just "integration1", proxyIntegration = Just True })
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "button", Selector.containing [ Selector.tag "span", Selector.classes [ "badge", "badge-danger" ], Selector.text "DELETE" ] ]
            , test "it should has a button contain a classes badge badge-danger when it action is put" <|
                \_ ->
                    Main.viewAction defaultParamViewAction (customViewModelAction { method = "PUT", vpcLink = Just "vpcLink", authorization = Just "authorization", integration = Just "integration1", proxyIntegration = Just True })
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "button", Selector.containing [ Selector.tag "span", Selector.classes [ "badge", "badge-warning" ], Selector.text "PUT" ] ]
            , test "it should hasNot a table with action information" <|
                \_ ->
                    Main.viewAction defaultParamViewAction defaultViewModelAction
                        |> Query.fromHtml
                        |> Query.hasNot [ Selector.tag "table" ]
            , test "it should has a table with action information" <|
                \_ ->
                    Main.viewAction defaultParamViewAction { defaultViewModelAction | isOpened = True }
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-bordered", "table-sm", "table-action-info" ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing [ Selector.tag "td", Selector.text "integration1" ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing [ Selector.tag "th", Selector.text "integration" ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing
                                    [ Selector.tag "td"
                                    , Selector.text "True"
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing [ Selector.tag "th", Selector.text "proxyIntegration" ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing
                                    [ Selector.tag "td"
                                    , Selector.text <|
                                        case defaultViewModelAction.action.vpcLink of
                                            Nothing ->
                                                "Propriedade não informada."

                                            Just vpcLink ->
                                                vpcLink
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing [ Selector.tag "th", Selector.text "vpcLink" ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing
                                    [ Selector.tag "td"
                                    , Selector.text <|
                                        case defaultViewModelAction.action.authorization of
                                            Nothing ->
                                                "Propriedade não informada."

                                            Just authorization ->
                                                authorization
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing [ Selector.tag "th", Selector.text "authorization" ]
                                ]
                            ]
            , test "it show up the msg Propriedade não informada. to properties not filled." <|
                \_ ->
                    let
                        cvma =
                            customViewModelAction { method = "GET", authorization = Nothing, vpcLink = Nothing, integration = Nothing, proxyIntegration = Nothing }
                    in
                    Main.viewAction defaultParamViewAction { cvma | isOpened = True }
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-bordered", "table-sm", "table-action-info" ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing [ Selector.class "authorization", Selector.tag "td", Selector.text "Propriedade não informada." ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing [ Selector.tag "th", Selector.text "integration" ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing
                                    [ Selector.tag "td"
                                    , Selector.class "proxyIntegration"
                                    , Selector.text "Propriedade não informada."
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing [ Selector.tag "th", Selector.text "proxyIntegration" ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing
                                    [ Selector.tag "td"
                                    , Selector.class "vpcLink"
                                    , Selector.text "Propriedade não informada."
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing [ Selector.tag "th", Selector.text "vpcLink" ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing
                                    [ Selector.tag "td"
                                    , Selector.class "authorization"
                                    , Selector.text "Propriedade não informada."
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing [ Selector.tag "th", Selector.text "authorization" ]
                                ]
                            ]
            , test "the table of information should has this classes " <|
                \_ ->
                    Main.viewAction defaultParamViewAction { defaultViewModelAction | isOpened = True }
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.all [ Selector.tag "table", Selector.classes [ "table", "table-bordered", "table-sm", "table-action-info" ] ]
                            ]
            , test "it should show up the action information " <|
                \_ ->
                    start
                        |> ProgramTest.clickButton "path"
                        |> ProgramTest.clickButton "GET"
                        |> ProgramTest.expectViewHas
                            [ Selector.all [ Selector.tag "th", Selector.containing [ Selector.text "proxyIntegration" ] ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing [ Selector.tag "th", Selector.text "authorization" ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing [ Selector.tag "th", Selector.text "vpcLink" ]
                                ]
                            ]
            , test "it should not show up the action information " <|
                \_ ->
                    start
                        |> ProgramTest.clickButton "path"
                        |> ProgramTest.clickButton "GET"
                        |> ProgramTest.clickButton "GET"
                        |> ProgramTest.expectViewHasNot
                            [ Selector.all [ Selector.tag "td", Selector.containing [ Selector.text "proxyIntegration" ] ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing [ Selector.tag "th", Selector.text "authorization" ]
                                ]
                            , Selector.all
                                [ Selector.tag "tr"
                                , Selector.containing [ Selector.tag "th", Selector.text "vpcLink" ]
                                ]
                            ]
            ]
        , describe "Test viewTextArea"
            [ test "it should be div with form-group containing text-area with form-control" <|
                \_ ->
                    Main.viewTextArea ""
                        |> div []
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.all [ Selector.tag "div", Selector.class "form-group", Selector.containing [ Selector.tag "textarea", Selector.attribute (HtmlAttr.rows 30), Selector.class "form-control" ] ]
                            , Selector.all [ Selector.tag "h3", Selector.containing [ Selector.text "API Routes File Configuration" ] ]
                            ]
            , test "it should load the rules from textArea" <|
                \_ ->
                    start
                        |> ProgramTest.fillInTextarea fileExample01Data
                        |> ProgramTest.expectViewHas
                            [ Selector.all [ Selector.tag "h2", Selector.containing [ Selector.text "fileExample01Data" ] ]
                            ]
            , test "it should showup the loaded json in textarea" <|
                \_ ->
                    startCustom "fileExample01Data"
                        |> ProgramTest.expectViewHas
                            [ Selector.all [ Selector.tag "textarea", Selector.attribute (HtmlAttr.value "fileExample01Data") ]
                            ]
            , test "it should just show up textarea when the json file is with problemn" <|
                \_ ->
                    start
                        |> ProgramTest.fillInTextarea """{"teste":"error"}"""
                        |> ProgramTest.expectViewHasNot
                            [ Selector.all [ Selector.tag "h2", Selector.containing [ Selector.text "fileExample01Data" ] ]
                            ]
            ]
        , describe "Test viewBlueprint"
            [ test "it should has a h2 with blueprint name and a paragraph with url_prefix." <|
                \_ ->
                    Main.viewBlueprint defaultBlueprint
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.all [ Selector.tag "h2", Selector.containing [ Selector.all [ Selector.tag "span", Selector.text "Blueprint: " ], Selector.text "Blueprint" ] ]
                            , Selector.all [ Selector.tag "p", Selector.containing [ Selector.all [ Selector.tag "span", Selector.text "Url Prefix: " ] ] ]
                            , Selector.all [ Selector.tag "p", Selector.containing [ Selector.text "url_prefix" ] ]
                            ]
            , test "it should has information about property nullable" <|
                \_ ->
                    Main.viewBlueprint { defaultBlueprint | name = Nothing, url_prefix = Nothing, resources = Nothing }
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.all [ Selector.tag "h2", Selector.containing [ Selector.all [ Selector.tag "span", Selector.text "Blueprint: " ] ] ]
                            , Selector.all [ Selector.tag "h2", Selector.containing [ Selector.all [ Selector.text "Propriedade não informada." ] ] ]
                            , Selector.all [ Selector.tag "p", Selector.containing [ Selector.all [ Selector.tag "span", Selector.text "Url Prefix: " ] ] ]
                            , Selector.all [ Selector.tag "p", Selector.containing [ Selector.text "Propriedade não informada." ] ]
                            , Selector.all [ Selector.tag "p", Selector.classes [ "alert", "alert-warning", "resources" ], Selector.containing [ Selector.text "Resources não informados." ] ]
                            ]
            ]
        , describe "Test viewJsonErrorReport"
            [ test "it should has a p with classes alert alert-warning." <|
                \_ ->
                    Main.viewJsonErrorReport defaultError
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.all [ Selector.tag "p", Selector.classes [ "alert", "alert-warning" ], Selector.containing [ Selector.text "Some error happend" ] ]
                            ]
            , test "it should has not the code of error." <|
                \_ ->
                    Main.viewJsonErrorReport defaultError
                        |> Query.fromHtml
                        |> Query.hasNot
                            [ Selector.all [ Selector.tag "p", Selector.classes [ "alert", "alert-warning" ], Selector.containing [ Selector.text "Problem with the given value:" ] ]
                            ]
            ]
        , describe "Test viewResourceFlask"
            [ test "it should has table with ths with title and tds with information for each attribute." <|
                \_ ->
                    Main.viewResourceFlask (Just defaultResourceFlask)
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "th", Selector.containing [ Selector.text "Resource Module" ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "td", Selector.containing [ Selector.text "resourceModule" ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "th", Selector.containing [ Selector.text "Resource Class" ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "td", Selector.containing [ Selector.text "resourceClass" ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "th", Selector.containing [ Selector.text "Strict Slashes" ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "td", Selector.containing [ Selector.text "Yes" ] ]
                                    ]
                                ]
                            ]
            , test "it should has table with th with title and td with information about nothing information for each attribute." <|
                \_ ->
                    Main.viewResourceFlask
                        (Just { defaultResourceFlask | resourceModule = Nothing, resourceClass = Nothing, strictSlashes = Nothing })
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "th", Selector.containing [ Selector.text "Resource Module" ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "td", Selector.classes [ "resourceModule" ], Selector.containing [ Selector.text "Propriedade não informada." ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "th", Selector.containing [ Selector.text "Resource Class" ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "td", Selector.classes [ "resourceClass" ], Selector.containing [ Selector.text "Propriedade não informada." ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "th", Selector.containing [ Selector.text "Strict Slashes" ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "td", Selector.classes [ "strictSlashes" ], Selector.containing [ Selector.text "Propriedade não informada." ] ]
                                    ]
                                ]
                            ]
            , test "it should has a p with information about resource flask is nothing." <|
                \_ ->
                    Main.viewResourceFlask Nothing
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.all
                                [ Selector.tag "p"
                                , Selector.classes [ "alert", "alert-warning" ]
                                , Selector.containing
                                    [ Selector.text "A propriedade flask não foi informada."
                                    ]
                                ]
                            ]
            ]
        , describe "Test viewCors"
            [ test "it should show up the phrase propriedade não informada para nothing variables" <|
                \_ ->
                    Main.viewCors (customCors { enable = Nothing, removeDefaultResponseTemplates = Nothing, allowHeaders = Nothing })
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered", "mt-2" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "td", Selector.class "removeDefaultResponseTemplates", Selector.containing [ Selector.text "Propriedade não informada." ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered", "mt-2" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "td", Selector.class "allowHeaders", Selector.containing [ Selector.text "Propriedade não informada." ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered", "mt-2" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "td", Selector.class "enable", Selector.containing [ Selector.text "Propriedade não informada." ] ]
                                    ]
                                ]
                            ]
            , test "it should has table with ths for each attribute." <|
                \_ ->
                    Main.viewCors defaultCors
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered", "mt-2" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "th", Selector.containing [ Selector.text "Enable" ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered", "mt-2" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "td", Selector.containing [ Selector.text "Yes" ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered", "mt-2" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "th", Selector.containing [ Selector.text "Remove Default Response Templates" ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered", "mt-2" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "td", Selector.containing [ Selector.text "Yes" ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered", "mt-2" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "th", Selector.containing [ Selector.text "Allow Headers" ] ]
                                    ]
                                ]
                            , Selector.all
                                [ Selector.tag "table"
                                , Selector.classes [ "table", "table-sm", "table-bordered", "mt-2" ]
                                , Selector.containing
                                    [ Selector.all [ Selector.tag "td", Selector.containing [ Selector.text "A, B, C" ] ]
                                    ]
                                ]
                            ]
            ]
        ]
