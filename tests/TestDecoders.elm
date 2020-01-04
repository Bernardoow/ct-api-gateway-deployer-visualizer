module TestDecoders exposing (decodersTests)

import Dict
import Expect exposing (Expectation)
import Json.Decode as Decode
import Models exposing (actionDecoder, blueprintDecoder, corsDecoder, methodDecoder, queryParamsDecoder, resourceDecoder, resourceFlaskDecoder, viewModelActionDecoder, viewViewModelMethod)
import Test exposing (..)
import TestData exposing (actionData, actionDataWithNulls, blueprintData, corsData, corsDataWithNulls, flaskData, methodData, methodDataWithQueryParamsNull, queryParamsData, resourceData)


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
                        , integration = Just "integration"
                        , proxyIntegration = Just True
                        , vpcLink = Just "vpcLink"
                        , authorization = Just "authorization"
                        }
                    )
        , test "Decoder Action WIth null" <|
            \_ ->
                let
                    result =
                        Decode.decodeString actionDecoder actionDataWithNulls
                            |> Result.toMaybe
                in
                Expect.equal result
                    (Just
                        { type_ = "GET"
                        , integration = Nothing
                        , proxyIntegration = Nothing
                        , vpcLink = Nothing
                        , authorization = Nothing
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
                            , integration = Just "integration"
                            , proxyIntegration = Just True
                            , vpcLink = Just "vpcLink"
                            , authorization = Just "authorization"
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
                Expect.equal result (Just { enable = Just True, removeDefaultResponseTemplates = Just True, allowHeaders = Just [ "header1" ] })
        , test "Decoder Cors With NUlls" <|
            \_ ->
                let
                    result =
                        Decode.decodeString corsDecoder corsDataWithNulls
                            |> Result.toMaybe
                in
                Expect.equal result (Just { enable = Nothing, removeDefaultResponseTemplates = Nothing, allowHeaders = Nothing })
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
                        , cors = { enable = Just True, removeDefaultResponseTemplates = Just True, allowHeaders = Just [ "header1" ] }
                        , queryParams = [ { name = "name", type_ = "type" } ]
                        , actions =
                            Dict.fromList
                                [ ( "GET"
                                  , { action =
                                        { type_ = "GET"
                                        , integration = Just "integration"
                                        , proxyIntegration = Just True
                                        , vpcLink = Just "vpcLink"
                                        , authorization = Just "authorization"
                                        }
                                    , isOpened = False
                                    }
                                  )
                                ]
                        }
                    )
        , test "Decoder Method with queryparams null" <|
            \_ ->
                let
                    result =
                        Decode.decodeString methodDecoder methodDataWithQueryParamsNull
                            |> Result.toMaybe
                in
                Expect.equal result
                    (Just
                        { path = "path"
                        , cors = { enable = Just True, removeDefaultResponseTemplates = Just True, allowHeaders = Just [ "header1" ] }
                        , queryParams = []
                        , actions =
                            Dict.fromList
                                [ ( "GET"
                                  , { action =
                                        { type_ = "GET"
                                        , integration = Just "integration"
                                        , proxyIntegration = Just True
                                        , vpcLink = Just "vpcLink"
                                        , authorization = Just "authorization"
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
                            , cors = { enable = Just True, removeDefaultResponseTemplates = Just True, allowHeaders = Just [ "header1" ] }
                            , queryParams = [ { name = "name", type_ = "type" } ]
                            , actions =
                                Dict.fromList
                                    [ ( "GET"
                                      , { action =
                                            { type_ = "GET"
                                            , integration = Just "integration"
                                            , proxyIntegration = Just True
                                            , vpcLink = Just "vpcLink"
                                            , authorization = Just "authorization"
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
                        Decode.decodeString resourceFlaskDecoder flaskData
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
                        , resourceFlask =
                            { resourceModule = "resourceModule"
                            , resourceClass = "resourceClass"
                            , strictSlashes = False
                            }
                        , methods =
                            Dict.fromList
                                [ ( "path"
                                  , { method =
                                        { path = "path"
                                        , cors = { enable = Just True, removeDefaultResponseTemplates = Just True, allowHeaders = Just [ "header1" ] }
                                        , queryParams = [ { name = "name", type_ = "type" } ]
                                        , actions =
                                            Dict.fromList
                                                [ ( "GET"
                                                  , { action =
                                                        { type_ = "GET"
                                                        , integration = Just "integration"
                                                        , proxyIntegration = Just True
                                                        , vpcLink = Just "vpcLink"
                                                        , authorization = Just "authorization"
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
                                    , resourceFlask =
                                        { resourceModule = "resourceModule"
                                        , resourceClass = "resourceClass"
                                        , strictSlashes = False
                                        }
                                    , methods =
                                        Dict.fromList
                                            [ ( "path"
                                              , { method =
                                                    { path = "path"
                                                    , cors = { enable = Just True, removeDefaultResponseTemplates = Just True, allowHeaders = Just [ "header1" ] }
                                                    , queryParams = [ { name = "name", type_ = "type" } ]
                                                    , actions =
                                                        Dict.fromList
                                                            [ ( "GET"
                                                              , { action =
                                                                    { type_ = "GET"
                                                                    , integration = Just "integration"
                                                                    , proxyIntegration = Just True
                                                                    , vpcLink = Just "vpcLink"
                                                                    , authorization = Just "authorization"
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
