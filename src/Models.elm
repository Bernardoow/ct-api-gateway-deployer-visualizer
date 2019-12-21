module Models exposing (Action, Blueprint, Flask, Method, Resource, ViewModelAction, ViewModelMethod, actionDecoder, blueprintDecoder, corsDecoder, flaskDecoder, methodDecoder, queryParamsDecoder, resourceDecoder, viewModelActionDecoder, viewViewModelMethod)

import Dict
import Json.Decode as Decode


type alias Action =
    { type_ : String
    , integration : String
    , proxyIntegration : Bool
    , vpcLink : String
    , authorization : String
    }


actionDecoder : Decode.Decoder Action
actionDecoder =
    Decode.map5 Action
        (Decode.field "type" Decode.string)
        (Decode.field "integration" Decode.string)
        (Decode.field "proxyIntegration" Decode.bool)
        (Decode.field "vpcLink" Decode.string)
        (Decode.field "authorization" Decode.string)


type alias ViewModelAction =
    { action : Action
    , isOpened : Bool
    }


viewModelActionDecoder : Decode.Decoder ViewModelAction
viewModelActionDecoder =
    Decode.map2 ViewModelAction
        actionDecoder
        (Decode.succeed False)


type alias QueryParam =
    { name : String
    , type_ : String
    }


queryParamsDecoder : Decode.Decoder QueryParam
queryParamsDecoder =
    Decode.map2 QueryParam
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string)


type alias Cors =
    { enable : Bool
    , removeDefaultResponseTemplates : Bool
    , allowHeaders : List String
    }


corsDecoder : Decode.Decoder Cors
corsDecoder =
    Decode.map3 Cors
        (Decode.field "enable" Decode.bool)
        (Decode.field "removeDefaultResponseTemplates" Decode.bool)
        (Decode.field "allowHeaders" (Decode.list Decode.string))


type alias Method =
    { path : String
    , cors : Cors
    , queryParams : List QueryParam
    , actions : List ViewModelAction
    }


methodDecoder : Decode.Decoder Method
methodDecoder =
    Decode.map4 Method
        (Decode.field "path" Decode.string)
        (Decode.field "cors" corsDecoder)
        (Decode.field "queryParams" (Decode.list queryParamsDecoder))
        (Decode.field "actions" (Decode.list viewModelActionDecoder))


type alias ViewModelMethod =
    { method : Method
    , isOpened : Bool
    }


viewViewModelMethod : Decode.Decoder ViewModelMethod
viewViewModelMethod =
    Decode.map2 ViewModelMethod
        methodDecoder
        (Decode.succeed False)


type alias Flask =
    { resourceModule : String
    , resourceClass : String
    , strictSlashes : Bool
    }


flaskDecoder : Decode.Decoder Flask
flaskDecoder =
    Decode.map3 Flask
        (Decode.field "resourceModule" Decode.string)
        (Decode.field "resourceClass" Decode.string)
        (Decode.field "strictSlashes" Decode.bool)


type alias Resource =
    { name : String
    , flask : Flask
    , methods : Dict.Dict String ViewModelMethod
    }


resourceDecoder : Decode.Decoder Resource
resourceDecoder =
    Decode.map3 Resource
        (Decode.field "name" Decode.string)
        (Decode.field "flask" flaskDecoder)
        (Decode.field "methods"
            (Decode.list viewViewModelMethod
                |> Decode.andThen (\methodsList -> List.map (\item -> ( item.method.path, item )) methodsList |> Dict.fromList |> Decode.succeed)
            )
        )


type alias Blueprint =
    { name : String
    , url_prefix : String
    , resources : Dict.Dict String Resource
    }


blueprintDecoder : Decode.Decoder Blueprint
blueprintDecoder =
    Decode.map3 Blueprint
        (Decode.field "name" Decode.string)
        (Decode.field "url_prefix" Decode.string)
        (Decode.field "resources" (Decode.list resourceDecoder |> Decode.andThen (\resourceList -> List.map (\resource -> ( resource.name, resource )) resourceList |> Dict.fromList |> Decode.succeed)))
