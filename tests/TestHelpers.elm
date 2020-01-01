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
                    Main.mountPathUrlUsingQueryParams [ { name = "age", type_ = "int" } ] |> Expect.equal "<int:age>"
            , test "it should be <int:age>/<string:name>" <|
                \_ ->
                    Main.mountPathUrlUsingQueryParams [ { name = "age", type_ = "int" }, { name = "name", type_ = "string" } ] |> Expect.equal "<int:age>/<string:name>"
            ]
        ]
