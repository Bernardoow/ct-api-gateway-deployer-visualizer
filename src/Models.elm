module Models exposing
    ( Action
    , Blueprint
    , Cors
    , Method
    , QueryParam
    , Resource
    , ResourceFlask
    , ViewModelAction
    , ViewModelMethod
    , actionDecoder
    , apiRoutesFileConfigurationDecoder
    , blueprintDecoder
    , corsDecoder
    , methodDecoder
    , queryParamsDecoder
    , resourceDecoder
    , resourceFlaskDecoder
    , viewModelActionDecoder
    , viewViewModelMethod
    )

import Dict
import Json.Decode as Decode



--type HttpMethods =
--    HM_GET
--    | HM_POST
--    | HM_


type alias Action =
    { type_ : String
    , integration : Maybe String
    , proxyIntegration : Maybe Bool
    , vpcLink : Maybe String
    , authorization : Maybe String
    }


actionDecoder : Decode.Decoder Action
actionDecoder =
    Decode.map5 Action
        (Decode.field "type" Decode.string)
        (Decode.maybe (Decode.field "integration" Decode.string))
        (Decode.maybe (Decode.field "proxyIntegration" Decode.bool))
        (Decode.maybe (Decode.field "vpcLink" Decode.string))
        (Decode.maybe (Decode.field "authorization" Decode.string))


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
    { name : Maybe String
    , type_ : Maybe String
    }


queryParamsDecoder : Decode.Decoder QueryParam
queryParamsDecoder =
    Decode.map2 QueryParam
        (Decode.maybe (Decode.field "name" Decode.string))
        (Decode.maybe (Decode.field "type" Decode.string))


type alias Cors =
    { enable : Maybe Bool
    , removeDefaultResponseTemplates : Maybe Bool
    , allowHeaders : Maybe (List String)
    }


corsDecoder : Decode.Decoder Cors
corsDecoder =
    Decode.map3 Cors
        (Decode.maybe (Decode.field "enable" Decode.bool))
        (Decode.maybe (Decode.field "removeDefaultResponseTemplates" Decode.bool))
        (Decode.maybe (Decode.field "allowHeaders" (Decode.list Decode.string)))


type alias Method =
    { path : String
    , cors : Maybe Cors
    , queryParams : List QueryParam
    , actions : Maybe (Dict.Dict String ViewModelAction)
    }


methodDecoder : Decode.Decoder Method
methodDecoder =
    Decode.map4 Method
        (Decode.field "path" Decode.string)
        (Decode.maybe (Decode.field "cors" corsDecoder))
        (Decode.field "queryParams" (Decode.oneOf [ Decode.list queryParamsDecoder, Decode.null [] ]))
        (Decode.maybe (Decode.field "actions" (Decode.list viewModelActionDecoder |> Decode.andThen (\modelActionList -> List.map (\modelAction -> ( modelAction.action.type_, modelAction )) modelActionList |> Dict.fromList |> Decode.succeed))))


type alias ViewModelMethod =
    { method : Method
    , isOpened : Bool
    }


viewViewModelMethod : Decode.Decoder ViewModelMethod
viewViewModelMethod =
    Decode.map2 ViewModelMethod
        methodDecoder
        (Decode.succeed False)


type alias ResourceFlask =
    { resourceModule : Maybe String
    , resourceClass : Maybe String
    , strictSlashes : Maybe Bool
    }


resourceFlaskDecoder : Decode.Decoder ResourceFlask
resourceFlaskDecoder =
    Decode.map3 ResourceFlask
        (Decode.maybe (Decode.field "resourceModule" Decode.string))
        (Decode.maybe (Decode.field "resourceClass" Decode.string))
        (Decode.maybe (Decode.field "strictSlashes" Decode.bool))


type alias Resource =
    { name : String
    , resourceFlask : Maybe ResourceFlask
    , methods : Maybe (Dict.Dict String ViewModelMethod)
    }


resourceDecoder : Decode.Decoder Resource
resourceDecoder =
    Decode.map3 Resource
        (Decode.field "name" Decode.string)
        (Decode.maybe (Decode.field "flask" resourceFlaskDecoder))
        (Decode.maybe
            (Decode.field "methods"
                (Decode.list viewViewModelMethod
                    |> Decode.andThen (\methodsList -> List.map (\item -> ( item.method.path, item )) methodsList |> Dict.fromList |> Decode.succeed)
                )
            )
        )


type alias Blueprint =
    { name : Maybe String
    , url_prefix : Maybe String
    , resources : Maybe (Dict.Dict String Resource)
    }


blueprintDecoder : Decode.Decoder Blueprint
blueprintDecoder =
    Decode.map3 Blueprint
        (Decode.maybe (Decode.field "name" Decode.string))
        (Decode.maybe (Decode.field "url_prefix" Decode.string))
        (Decode.maybe
            (Decode.field "resources"
                (Decode.list resourceDecoder
                    |> Decode.andThen
                        (\resourceList ->
                            List.map (\resource -> ( resource.name, resource )) resourceList
                                |> Dict.fromList
                                |> Decode.succeed
                        )
                )
            )
        )


apiRoutesFileConfigurationDecoder : Decode.Decoder Blueprint
apiRoutesFileConfigurationDecoder =
    Decode.field "blueprint" blueprintDecoder
