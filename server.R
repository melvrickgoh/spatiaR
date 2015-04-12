source("helpers.R")
source("db.R")
source("spatia.R")

shinyServer(function(input, output, session) {

  output$uuid <- renderText({
    if (session$clientData$url_pathname == "/") {
      query <- parseQueryString(session$clientData$url_search)
      paste(query)
      paste(query$uuid)
    }
  })

  output$user <- renderText({
    if (session$clientData$url_pathname == "/") {
      query <- parseQueryString(session$clientData$url_search)
      paste(query)
      paste(query$user)
    }
  })

  output$kde <- renderPlot({
    results = 'hullo'
    if (session$clientData$url_pathname == "/") {
      query <- parseQueryString(session$clientData$url_search)
      print(query$action == 'kde')
      if (query$action == 'kde') {
        uuid = query$uuid
        userid = query$user

        payload = dbKDEGroup(userid,uuid)
        results = payload

        spatialPoints = dfToSpatialPoints(payload)

        spatialPointsFrame = spatialPoints$dataframe

        contourKDE(spatialPoints,userid,uuid)

        lfunctionplot = lfunctionKDE(spatialPoints,userid,uuid)
        
        paste(lfunctionplot)
      }
    }
  },width=400,height=400,res=72)

  # Return the components of the URL in a string:

  output$urlText <- renderText({
    paste(sep = "",
      "protocol: ", session$clientData$url_protocol, "\n",
      "hostname: ", session$clientData$url_hostname, "\n",
      "pathname: ", session$clientData$url_pathname, "\n",
      "port: ",     session$clientData$url_port,     "\n",
      "search: ",   session$clientData$url_search,   "\n"
    )
  })

  # Parse the GET query string
  output$queryText <- renderText({
    query <- parseQueryString(session$clientData$url_search)

    # Return a string with key-value pairs
    paste(names(query), query, sep = "=", collapse=", ")
  })
})