module TestDecoders exposing (decodersTests)

import Dict
import Expect exposing (Expectation)
import Json.Decode as Decode
import Models exposing (actionDecoder, blueprintDecoder, corsDecoder, flaskDecoder, methodDecoder, queryParamsDecoder, resourceDecoder, viewModelActionDecoder, viewViewModelMethod)
import Test exposing (..)
import TestData exposing (actionData, blueprintData, corsData, flaskData, methodData, queryParamsData, resourceData)


decodersTests : Test
decodersTests =
    describe "Decoders"
        [ test "Decoder Action" <|
            \_ ->
                let
                    result =
                        Decode.decodeString actionDecoder actionData
                            |> Result.toMaybe
                in
                Expect.equal result
                    (Just
                        { type_ = "GET"
                        , integration = "integration"
                        , proxyIntegration = True
                        , vpcLink = "vpcLink"
                        , authorization = "authorization"
                        }
                    )
        , test "Decoder ViewModelAction" <|
            \_ ->
                let
                    result =
                        Decode.decodeString viewModelActionDecoder actionData
                            |> Result.toMaybe
                in
                Expect.equal result
                    (Just
                        { action =
                            { type_ = "GET"
                            , integration = "integration"
                            , proxyIntegration = True
                            , vpcLink = "vpcLink"
                            , authorization = "authorization"
                            }
                        , isOpened = False
                        }
                    )
        , test "Decoder QueryParams" <|
            \_ ->
                let
                    result =
                        Decode.decodeString queryParamsDecoder queryParamsData
                            |> Result.toMaybe
                in
                Expect.equal result (Just { name = "name", type_ = "type" })
        , test "Decoder Cors" <|
            \_ ->
                let
                    result =
                        Decode.decodeString corsDecoder corsData
                            |> Result.toMaybe
                in
                Expect.equal result (Just { enable = True, removeDefaultResponseTemplates = True, allowHeaders = [ "header1" ] })
        , test "Decoder Method" <|
            \_ ->
                let
                    result =
                        Decode.decodeString methodDecoder methodData
                            |> Result.toMaybe
                in
                Expect.equal result
                    (Just
                        { path = "path"
                        , cors = { enable = True, removeDefaultResponseTemplates = True, allowHeaders = [ "header1" ] }
                        , queryParams = [ { name = "name", type_ = "type" } ]
                        , actions =
                            Dict.fromList
                                [ ( "GET"
                                  , { action =
                                        { type_ = "GET"
                                        , integration = "integration"
                                        , proxyIntegration = True
                                        , vpcLink = "vpcLink"
                                        , authorization = "authorization"
                                        }
                                    , isOpened = False
                                    }
                                  )
                                ]
                        }
                    )
        , test "Decoder viewViewModelMethod" <|
            \_ ->
                let
                    result =
                        Decode.decodeString viewViewModelMethod methodData
                            |> Result.toMaybe
                in
                Expect.equal result
                    (Just
                        { method =
                            { path = "path"
                            , cors = { enable = True, removeDefaultResponseTemplates = True, allowHeaders = [ "header1" ] }
                            , queryParams = [ { name = "name", type_ = "type" } ]
                            , actions =
                                Dict.fromList
                                    [ ( "GET"
                                      , { action =
                                            { type_ = "GET"
                                            , integration = "integration"
                                            , proxyIntegration = True
                                            , vpcLink = "vpcLink"
                                            , authorization = "authorization"
                                            }
                                        , isOpened = False
                                        }
                                      )
                                    ]
                            }
                        , isOpened = False
                        }
                    )
        , test "Decoder Flask" <|
            \_ ->
                let
                    result =
                        Decode.decodeString flaskDecoder flaskData
                            |> Result.toMaybe
                in
                Expect.equal result
                    (Just
                        { resourceModule = "resourceModule"
                        , resourceClass = "resourceClass"
                        , strictSlashes = False
                        }
                    )
        , test "Decoder Resource" <|
            \_ ->
                let
                    result =
                        Decode.decodeString resourceDecoder resourceData
                            |> Result.toMaybe
                in
                Expect.equal result
                    (Just
                        { name = "name"
                        , flask =
                            { resourceModule = "resourceModule"
                            , resourceClass = "resourceClass"
                            , strictSlashes = False
                            }
                        , methods =
                            Dict.fromList
                                [ ( "path"
                                  , { method =
                                        { path = "path"
                                        , cors = { enable = True, removeDefaultResponseTemplates = True, allowHeaders = [ "header1" ] }
                                        , queryParams = [ { name = "name", type_ = "type" } ]
                                        , actions =
                                            Dict.fromList
                                                [ ( "GET"
                                                  , { action =
                                                        { type_ = "GET"
                                                        , integration = "integration"
                                                        , proxyIntegration = True
                                                        , vpcLink = "vpcLink"
                                                        , authorization = "authorization"
                                                        }
                                                    , isOpened = False
                                                    }
                                                  )
                                                ]
                                        }
                                    , isOpened = False
                                    }
                                  )
                                ]
                        }
                    )
        , test "Decoder Blueprint" <|
            \_ ->
                let
                    result =
                        Decode.decodeString blueprintDecoder blueprintData
                            |> Result.toMaybe
                in
                Expect.equal result
                    (Just
                        { name = "name"
                        , url_prefix = "url_prefix"
                        , resources =
                            Dict.fromList
                                [ ( "name"
                                  , { name = "name"
                                    , flask =
                                        { resourceModule = "resourceModule"
                                        , resourceClass = "resourceClass"
                                        , strictSlashes = False
                                        }
                                    , methods =
                                        Dict.fromList
                                            [ ( "path"
                                              , { method =
                                                    { path = "path"
                                                    , cors = { enable = True, removeDefaultResponseTemplates = True, allowHeaders = [ "header1" ] }
                                                    , queryParams = [ { name = "name", type_ = "type" } ]
                                                    , actions =
                                                        Dict.fromList
                                                            [ ( "GET"
                                                              , { action =
                                                                    { type_ = "GET"
                                                                    , integration = "integration"
                                                                    , proxyIntegration = True
                                                                    , vpcLink = "vpcLink"
                                                                    , authorization = "authorization"
                                                                    }
                                                                , isOpened = False
                                                                }
                                                              )
                                                            ]
                                                    }
                                                , isOpened = False
                                                }
                                              )
                                            ]
                                    }
                                  )
                                ]
                        }
                    )
        ]
