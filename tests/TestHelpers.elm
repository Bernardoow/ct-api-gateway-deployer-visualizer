module TestHelpers exposing (testsHelpers)

import Expect exposing (Expectation)
import Main as Main
import Test exposing (Test, describe, test)


testsHelpers : Test
testsHelpers =
    describe "Tests Helpers"
        [ describe "Test mountPathUrlUsingQueryParams"
            [ test "it should be empty string" <|
                \_ ->
                    Main.mountPathUrlUsingQueryParams [] |> Expect.equal ""
            , test "it should be <int:age>" <|
                \_ ->
                    Main.mountPathUrlUsingQueryParams [ { name = Just "age", type_ = Just "int" } ] |> Expect.equal "<int:age>"
            , test "it should be <int:age>/<string:name>" <|
                \_ ->
                    Main.mountPathUrlUsingQueryParams [ { name = Just "age", type_ = Just "int" }, { name = Just "name", type_ = Just "string" } ] |> Expect.equal "<int:age>/<string:name>"
            , test "test queryparams without type_ it should be <Parâmetro tipo não informado.:age>" <|
                \_ ->
                    Main.mountPathUrlUsingQueryParams [ { name = Just "age", type_ = Nothing } ] |> Expect.equal "<Parâmetro tipo não informado.:age>"
            , test "test queryparams without nome it should be <int:Parâmetro nome não informado.>" <|
                \_ ->
                    Main.mountPathUrlUsingQueryParams [ { name = Nothing, type_ = Just "int" } ] |> Expect.equal "<int:Parâmetro nome não informado.>"
            ]
        ]
