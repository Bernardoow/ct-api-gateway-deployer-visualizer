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
import TestData exposing (actionData, corsData, fileExample01Data, flaskData, methodData, queryParamsData, resourceData)


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


customViewModelAction : String -> ViewModelAction
customViewModelAction method =
    { action =
        { type_ = method
        , integration = "integration1"
        , proxyIntegration = True
        , vpcLink = "vpcLink1"
        , authorization = "authorization1"
        }
    , isOpened = False
    }


defaultViewModelAction : ViewModelAction
defaultViewModelAction =
    customViewModelAction "GET"


defaultParamViewAction : { resourcekey : String, methodKey : String }
defaultParamViewAction =
    { resourcekey = "String", methodKey = "String" }


defaultMethod : Method
defaultMethod =
    { path = "path"
    , cors = { enable = True, removeDefaultResponseTemplates = True, allowHeaders = [ "header1" ] }
    , queryParams = [ { name = "name", type_ = "type" } ]
    , actions =
        Dict.fromList
            [ ( "GET", defaultViewModelAction )
            ]
    }


defaultBlueprint : Blueprint
defaultBlueprint =
    { name = "Blueprint"
    , url_prefix = "url_prefix"
    , resources = Dict.empty
    }


defaultError : Decode.Error
defaultError =
    Decode.Failure "Some error happend" (Encode.int 2)


defaultResourceFlask : ResourceFlask
defaultResourceFlask =
    { resourceModule = "resourceModule"
    , resourceClass = "resourceClass"
    , strictSlashes = True
    }


defaultCors : Cors
defaultCors =
    { enable = True
    , removeDefaultResponseTemplates = True
    , allowHeaders = [ "A", "B", "C" ]
    }


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
        , describe "Test viewResource"
            [ test "it should has a title h3" <|
                \_ ->
                    Main.viewResource { name = "Resource 1", resourceFlask = defaultResourceFlask, methods = Dict.empty }
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "h3", Selector.containing [ Selector.text "Resource 1" ] ]
            , test "it should has a div with class list-group" <|
                \_ ->
                    Main.viewResource { name = "Resource 1", resourceFlask = defaultResourceFlask, methods = Dict.empty }
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "div", Selector.classes [ "list-group", "col-12" ] ]
            , test "it should has a table for resourceFlask" <|
                \_ ->
                    Main.viewResource { name = "Resource 1", resourceFlask = defaultResourceFlask, methods = Dict.empty }
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "table", Selector.classes [ "table", "table-sm", "table-bordered" ] ]
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
                    Main.viewAction defaultParamViewAction (customViewModelAction "GET")
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "button", Selector.containing [ Selector.tag "span", Selector.classes [ "badge", "badge-info" ], Selector.text "GET" ] ]
            , test "it should has a button contain a classes badge badge-success when it action is post" <|
                \_ ->
                    Main.viewAction defaultParamViewAction (customViewModelAction "POST")
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "button", Selector.containing [ Selector.tag "span", Selector.classes [ "badge", "badge-success" ], Selector.text "POST" ] ]
            , test "it should has a button contain a classes badge badge-danger when it action is delete" <|
                \_ ->
                    Main.viewAction defaultParamViewAction (customViewModelAction "DELETE")
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "button", Selector.containing [ Selector.tag "span", Selector.classes [ "badge", "badge-danger" ], Selector.text "DELETE" ] ]
            , test "it should has a button contain a classes badge badge-danger when it action is put" <|
                \_ ->
                    Main.viewAction defaultParamViewAction (customViewModelAction "PUT")
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "button", Selector.containing [ Selector.tag "span", Selector.classes [ "badge", "badge-warning" ], Selector.text "PUT" ] ]
            , test "it should hasNot a table with action information" <|
                \_ ->
                    Main.viewAction defaultParamViewAction defaultViewModelAction
                        |> Query.fromHtml
                        |> Query.hasNot [ Selector.tag "table" ]
            , test "it should has a table with action information" <|
                \_ ->
                    Main.viewAction defaultParamViewAction defaultViewModelAction
                        |> Query.fromHtml
                        |> Query.findAll [ Selector.tag "table" ]
                        |> Query.each
                            (Expect.all
                                [ Query.has
                                    [ Selector.all
                                        [ Selector.tag "table"
                                        , Selector.classes [ "table", "table-bordered", "table-sm", "table-action-info" ]
                                        ]
                                    ]
                                , Query.has
                                    [ Selector.all
                                        [ Selector.tag "tr"
                                        , Selector.containing [ Selector.tag "td", Selector.text defaultViewModelAction.action.integration ]
                                        ]
                                    , Selector.all
                                        [ Selector.tag "tr"
                                        , Selector.containing [ Selector.tag "th", Selector.text "integration" ]
                                        ]
                                    ]
                                , Query.has
                                    [ Selector.all
                                        [ Selector.tag "tr"
                                        , Selector.containing
                                            [ Selector.tag "td"
                                            , Selector.text <|
                                                if defaultViewModelAction.action.proxyIntegration then
                                                    "True"

                                                else
                                                    "False"
                                            ]
                                        ]
                                    , Selector.all
                                        [ Selector.tag "tr"
                                        , Selector.containing [ Selector.tag "th", Selector.text "proxyIntegration" ]
                                        ]
                                    ]
                                , Query.has
                                    [ Selector.all
                                        [ Selector.tag "tr"
                                        , Selector.containing [ Selector.tag "td", Selector.text defaultViewModelAction.action.vpcLink ]
                                        ]
                                    , Selector.all
                                        [ Selector.tag "tr"
                                        , Selector.containing [ Selector.tag "th", Selector.text "vpcLink" ]
                                        ]
                                    ]
                                , Query.has
                                    [ Selector.all
                                        [ Selector.tag "tr"
                                        , Selector.containing [ Selector.tag "td", Selector.text defaultViewModelAction.action.authorization ]
                                        ]
                                    , Selector.all
                                        [ Selector.tag "tr"
                                        , Selector.containing [ Selector.tag "th", Selector.text "authorization" ]
                                        ]
                                    ]
                                ]
                            )
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
            [ test "it should has table with ths for each attribute." <|
                \_ ->
                    Main.viewResourceFlask defaultResourceFlask
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
            ]
        , describe "Test viewCors"
            [ test "it should has table with ths for each attribute." <|
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
