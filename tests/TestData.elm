module TestData exposing
    ( actionData
    , actionDataWithNulls
    , blueprintData
    , blueprintDataWithNulls
    , corsData
    , corsDataWithNulls
    , fileExample01Data
    , methodData
    , methodDataWithInvalidQueryParams
    , methodDataWithQueryParamsNull
    , queryParamsData
    , queryParamsDataWithNulls
    , resourceData
    , resourceDataWithNulls
    , resourceFlaskData
    , resourceFlaskDataWithNulls
    )


actionData : String
actionData =
    """{"type": "GET", "integration": "integration", "proxyIntegration": true, "vpcLink": "vpcLink", "authorization": "authorization"}"""


actionDataWithNulls : String
actionDataWithNulls =
    """{"type": "GET", "integration": null, "proxyIntegration": null, "vpcLink": null, "authorization": null}"""


queryParamsData : String
queryParamsData =
    """{"name": "name", "type": "type"} """


queryParamsDataWithNulls : String
queryParamsDataWithNulls =
    """{"name": "name"} """


corsData : String
corsData =
    """{"enable": true, "removeDefaultResponseTemplates": true, "allowHeaders": ["header1"]} """


corsDataWithNulls : String
corsDataWithNulls =
    """{"enable": null, "removeDefaultResponseTemplates": null, "allowHeaders": null} """


methodData : String
methodData =
    """{"path": "path", "cors": """ ++ corsData ++ """, "queryParams": [""" ++ queryParamsData ++ """], "actions": [""" ++ actionData ++ """] }"""


methodDataWithInvalidQueryParams : String
methodDataWithInvalidQueryParams =
    """{"path": "path", "cors": """ ++ corsData ++ """, "queryParams": [""" ++ queryParamsDataWithNulls ++ """], "actions": [""" ++ actionData ++ """] }"""


methodDataWithQueryParamsNull : String
methodDataWithQueryParamsNull =
    """{"path": "path",  "queryParams": null}"""


resourceFlaskData : String
resourceFlaskData =
    """{"resourceModule": "resourceModule", "resourceClass": "resourceClass", "strictSlashes": false}"""


resourceFlaskDataWithNulls : String
resourceFlaskDataWithNulls =
    """{"resourceModule": null, "resourceClass": null}"""


resourceData : String
resourceData =
    """{"name": "name", "flask": """ ++ resourceFlaskData ++ """, "methods": [""" ++ methodData ++ """]}"""


resourceDataWithNulls : String
resourceDataWithNulls =
    """{"name": "name"}"""


blueprintData : String
blueprintData =
    """{"name": "name", "url_prefix": "url_prefix", "resources": [""" ++ resourceData ++ """]}"""


blueprintDataWithNulls : String
blueprintDataWithNulls =
    """{"name": null, "url_prefix": null}"""


fileExample01Data : String
fileExample01Data =
    """
    {
    "blueprint" : {
        "name": "fileExample01Data",
        "url_prefix": "url_prefix",
        "resources": [{
            "name": "get-users1",
            "flask": {
                "resourceModule": "resources",
                "resourceClass": "GetUsers",
                "strictSlashes": false
            },
            "methods": [{
                "path": "/path",
                "cors": {
                  "enable": true, 
                  "removeDefaultResponseTemplates": true,
                  "allowHeaders": [""]
                },
                "queryParams": [{
                   "name": "teste",
                   "type": "string"
                }],
                "actions": [{
                    "type": "GET",
                    "integration": "",
                    "proxyIntegration": true,
                    "vpcLink": "",
                    "authorization": ""
                }]
            }]
        }]
    }
}


    """
