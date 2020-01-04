module TestData exposing (actionData, actionDataWithNulls, blueprintData, corsData, corsDataWithNulls, fileExample01Data, flaskData, methodData, methodDataWithQueryParamsNull, queryParamsData, resourceData)


actionData : String
actionData =
    """{"type": "GET", "integration": "integration", "proxyIntegration": true, "vpcLink": "vpcLink", "authorization": "authorization"}"""


actionDataWithNulls : String
actionDataWithNulls =
    """{"type": "GET", "integration": null, "proxyIntegration": null, "vpcLink": null, "authorization": null}"""


queryParamsData : String
queryParamsData =
    """{"name": "name", "type": "type"} """


corsData : String
corsData =
    """{"enable": true, "removeDefaultResponseTemplates": true, "allowHeaders": ["header1"]} """


corsDataWithNulls : String
corsDataWithNulls =
    """{"enable": null, "removeDefaultResponseTemplates": null, "allowHeaders": null} """


methodData : String
methodData =
    """{"path": "path", "cors": """ ++ corsData ++ """, "queryParams": [""" ++ queryParamsData ++ """], "actions": [""" ++ actionData ++ """] }"""


methodDataWithQueryParamsNull : String
methodDataWithQueryParamsNull =
    """{"path": "path", "cors": """ ++ corsData ++ """, "queryParams": null, "actions": [""" ++ actionData ++ """] }"""


flaskData : String
flaskData =
    """{"resourceModule": "resourceModule", "resourceClass": "resourceClass", "strictSlashes": false}"""


resourceData : String
resourceData =
    """{"name": "name", "flask": """ ++ flaskData ++ """, "methods": [""" ++ methodData ++ """]}"""


blueprintData : String
blueprintData =
    """{"name": "name", "url_prefix": "url_prefix", "resources": [""" ++ resourceData ++ """]}"""


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
