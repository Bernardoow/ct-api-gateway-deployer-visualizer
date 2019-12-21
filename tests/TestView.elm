module TestView exposing (viewsTests)

import Dict
import Expect exposing (Expectation)
import Json.Decode as Decode
import Main as Main
import Models exposing (Flask, Method, ViewModelAction, actionDecoder, blueprintDecoder, corsDecoder, flaskDecoder, methodDecoder, queryParamsDecoder, resourceDecoder)
import ProgramTest exposing (ProgramTest)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import TestData exposing (actionData, blueprintData, corsData, flaskData, methodData, queryParamsData, resourceData)


start : ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
start =
    ProgramTest.createElement
        { init = \_ -> Main.init
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.start ()


defaultFlask : Flask
defaultFlask =
    { resourceModule = "resourceModule"
    , resourceClass = "resourceClass"
    , strictSlashes = False
    }


defaultViewModelAction : ViewModelAction
defaultViewModelAction =
    { action =
        { type_ = "GET"
        , integration = "integration1"
        , proxyIntegration = True
        , vpcLink = "vpcLink1"
        , authorization = "authorization1"
        }
    , isOpened = False
    }


defaultMethod : Method
defaultMethod =
    { path = "path"
    , cors = { enable = True, removeDefaultResponseTemplates = True, allowHeaders = [ "header1" ] }
    , queryParams = [ { name = "name", type_ = "type" } ]
    , actions =
        [ defaultViewModelAction
        ]
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
                    Main.viewResource { name = "Resource 1", flask = defaultFlask, methods = Dict.empty }
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "h3", Selector.containing [ Selector.text "Resource 1" ] ]
            , test "it should has a div with class list-group" <|
                \_ ->
                    Main.viewResource { name = "Resource 1", flask = defaultFlask, methods = Dict.empty }
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "div", Selector.class "list-group" ]
            ]
        , describe "Test viewMethod"
            [ test "it should has a button with method information" <|
                \_ ->
                    Main.viewMethod "" { method = { defaultMethod | path = "endpointA/" }, isOpened = False }
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "button", Selector.containing [ Selector.text "endpointA/" ] ]
            , test "it should not show up the information about actions" <|
                \_ ->
                    Main.viewMethod "" { method = { defaultMethod | path = "endpointA/" }, isOpened = False }
                        |> Query.fromHtml
                        |> Query.hasNot [ Selector.tag "div", Selector.class "list-group" ]
            , test "it should show up the information about actions" <|
                \_ ->
                    Main.viewMethod "" { method = { defaultMethod | path = "endpointA/" }, isOpened = True }
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
                    Main.viewAction defaultViewModelAction
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "button", Selector.containing [ Selector.text "GET" ] ]
            , test "it should hasNot a table with action information" <|
                \_ ->
                    Main.viewAction defaultViewModelAction
                        |> Query.fromHtml
                        |> Query.hasNot [ Selector.tag "table" ]
            , test "it should has a table with action information" <|
                \_ ->
                    Main.viewAction defaultViewModelAction
                        |> Query.fromHtml
                        |> Query.findAll [ Selector.tag "table" ]
                        |> Query.each
                            (Expect.all
                                [ Query.has
                                    [ Selector.tag "table"
                                    ]
                                , Query.has
                                    [ Selector.all
                                        [ Selector.tag "tr"
                                        , Selector.containing [ Selector.tag "td", Selector.text defaultViewModelAction.action.integration ]
                                        ]
                                    , Selector.all
                                        [ Selector.tag "tr"
                                        , Selector.containing [ Selector.tag "td", Selector.text "integration" ]
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
                                        , Selector.containing [ Selector.tag "td", Selector.text "proxyIntegration" ]
                                        ]
                                    ]
                                , Query.has
                                    [ Selector.all
                                        [ Selector.tag "tr"
                                        , Selector.containing [ Selector.tag "td", Selector.text defaultViewModelAction.action.vpcLink ]
                                        ]
                                    , Selector.all
                                        [ Selector.tag "tr"
                                        , Selector.containing [ Selector.tag "td", Selector.text "vpcLink" ]
                                        ]
                                    ]
                                , Query.has
                                    [ Selector.all
                                        [ Selector.tag "tr"
                                        , Selector.containing [ Selector.tag "td", Selector.text defaultViewModelAction.action.authorization ]
                                        ]
                                    , Selector.all
                                        [ Selector.tag "tr"
                                        , Selector.containing [ Selector.tag "td", Selector.text "authorization" ]
                                        ]
                                    ]
                                ]
                            )
            ]
        ]
