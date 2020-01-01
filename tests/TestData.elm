module TestData exposing (actionData, blueprintData, corsData, fileExample01Data, flaskData, methodData, queryParamsData, resourceData)


actionData : String
actionData =
    """{"type": "GET", "integration": "integration", "proxyIntegration": true, "vpcLink": "vpcLink", "authorization": "authorization"}"""


queryParamsData : String
queryParamsData =
    """{"name": "name", "type": "type"} """


corsData : String
corsData =
    """{"enable": true, "removeDefaultResponseTemplates": true, "allowHeaders": ["header1"]} """


methodData : String
methodData =
    """{"path": "path", "cors": """ ++ corsData ++ """, "queryParams": [""" ++ queryParamsData ++ """], "actions": [""" ++ actionData ++ """] }"""


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
