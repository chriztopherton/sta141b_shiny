library(nycflights13)
library(shiny)
library(tidyverse)
library(shinythemes)
library(leaflet)


library(ggplot2)
library(tidyverse)
library(plotly)
#install.packages('devtools')
#devtools::install_github("michaeldorman/mapsapi")
#devtools::install_github("dkahle/ggmap")
library(mapsapi)
library(ggmap)
library(maps)
library(mapproj)
library(leaflet)
library(shiny)
library(shinydashboard)
library(rsconnect)
library(rromeo)
library(shinyWidgets)
library(tidytext)
library(jsonlite)
## install rtweet from CRAN
library(httr)
library(rtweet)
#----------------------------------------------------------------------------------------------------
#register for a free google maps api key at https://cloud.google.com/maps-platform/pricing
maps_key = Sys.getenv("maps_key")
register_google(key = maps_key)

news_key = Sys.getenv("news_key")

yelp_client_ID = Sys.getenv("yelp_client_ID")
yelp_key = Sys.getenv("yelp_key")

news_key = Sys.getenv("news_key")

#---------------------------------------------------------------------------------------------------------
createLink <- function(val) {
  sprintf('<a href="" target="_blank" class="btn btn-primary">Info</a>',val)
}
#---------------------------------------------------------------------------------------------------------


ui <- fluidPage(theme=shinytheme("darkly"),
                
                titlePanel(h1("TRAVELER'S GUIDE",align="center",style='background-color:teal;
                     padding-left: 15px',tags$img(height = 50,
                                                  width=70,
                                                  src ="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f0/Icon-notepad.svg/1024px-Icon-notepad.svg.png"))),
                br(),
                
                # user input for Point A
                sidebarPanel
                (
                  h1("Explore",align="center",style='background-color:teal;
                     padding-left: 5px',  tags$img(height = 250,
                                                   width=300,
                                                   src = "https://media.cntraveler.com/photos/59cd14cb9465da68882fb4f4/master/pass/Debate_GettyImages-585587819.jpg")),
                  hr(),
                  
                  searchInput
                  (
                    inputId = "origin", h2("Enter:",
                                           tags$img(height = 50,
                                                    width=50,
                                                    src ="https://image.flaticon.com/icons/svg/1531/1531126.svg")),
                    value = "Davis",btnSearch = icon("search"),
                    btnReset = icon("remove"),width = "450px"
                  ),
                  
                  
                  searchInput
                  (
                    inputId = "destination",label = h2("Enter:",
                                                       tags$img(height = 50,
                                                                width=50,
                                                                src ="https://www.iconbunny.com/icons/media/catalog/product/2/6/2657.9-finish-icon-iconbunny.jpg")),
                    value = "Sacramento",btnSearch = icon("search"),
                    btnReset = icon("remove"),width = "450px"
                  ),
                  
                  tableOutput(outputId = "dist_dur")
                ),
                
                #----------------------------------------------------------------------------------------------------
                mainPanel
                (
                  column(6,h2("Google Maps",align="center",tags$img(height = 50,
                                                                    width=70,
                                                                    src ="https://www.onlinemarketingwhiz.com.au/wp-content/uploads/2018/07/google-maps-ios-icon-top.png")),
                         leafletOutput(outputId = "map",
                                       width="100%",
                                       height = "1500")),
                  #tableOutput(outputId = "distancetable"),
                  column(6,
                         tabsetPanel
                         (
                           tabPanel(h3("Yelp",tags$img(height = 50,
                                                       width=50,
                                                       src ="https://upload.wikimedia.org/wikipedia/commons/thumb/e/e8/Yelp.svg/1024px-Yelp.svg.png")),
                                    dataTableOutput(outputId = "places")),
                           tabPanel(h3("Twitter",tags$img(height = 50,
                                                          width=50,
                                                          src ="https://cdn2.iconfinder.com/data/icons/social-media-2285/512/1_Twitter_colored_svg-512.png")),
                                    dataTableOutput(outputId = "tweets")),
                           tabPanel(h3("News",tags$img(height = 50,
                                                       width=50,
                                                       src ="https://img.favpng.com/20/25/11/newspaper-computer-icons-symbol-png-favpng-uxcxrxULJwf1TaT4zWQDsFUcw.jpg")),
                                    dataTableOutput(outputId = "news"))
                         )
                  ))
)


#------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  doc = reactive({mp_directions(origin = input$origin, #from UCD Pav
                                destination = input$destination,
                                mode = c("driving", "transit", "walking", "bicycling"),
                                alternatives = FALSE,
                                waypoints = NULL,
                                key = paste(maps_key))
  })
  
  
  #the lines below can be commented out to prevent API requests
  
  #maps + route
  
  #given response object, use mp_get_routes to create a spatial layer of route lines
  
  
  r = reactive({mp_get_routes(doc())})
  
  output$dist_dur <- renderTable({
    c = data.frame(c("Distance",r()$distance_text),c("Duration",r()$duration_text))
    names(c) = c("","")
    c
  })
  
  seg = reactive({mp_get_segments(doc())})
  
  output$map <- renderLeaflet({
    req(doc())
    
    p1 = c(geocode(paste(input$origin))$lat,geocode(paste(input$origin))$lon)
    p2 = c(geocode(paste(input$destination))$lat,geocode(paste(input$destination))$lon)
    coord = data.frame(p1,p2)
    
    return(leaflet(seg()) %>%
             addProviderTiles(providers$Esri.WorldStreetMap) %>%
             addPolylines(opacity = 3,
                          weight = 7,
                          #color = ~pal()(seg()$segment_id),
                          color = "blue",
                          popup = ~seg()$instructions) %>%
             addTiles() %>% addMarkers(lng = p1[2],lat = p1[1],
                                       label = paste(input$origin),
                                       labelOptions = labelOptions(noHide = T,
                                                                   textsize = "15px")) %>%
             addMarkers(lng = p2[2],lat = p2[1],
                        label = paste(input$destination),
                        labelOptions = labelOptions(noHide = T,
                                                    textsize = "15px")))
  })
  
  
  
  #------------------------------------------------------------------------------------------------------------------------
  
  
  output$places <- renderDataTable({
    
    
    
    #create token with provided id, key
    res <- POST("https://api.yelp.com/oauth2/token",
                body = list(grant_type = "client_credentials",
                            client_id = yelp_client_ID,
                            client_secret = yelp_key))
    
    token <- content(res)$access_token
    
    yelp <- "https://api.yelp.com"
    term <- "cookies"
    location <- paste(input$destination)
    categories <- NULL
    limit <- 50
    radius <- 1000
    url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                      query = list(term = term, location = location, 
                                   limit = limit,
                                   radius = radius))
    res <- GET(url, add_headers('Authorization' = paste("Bearer", yelp_key)))
    
    results <- content(res,as="text")
    
    yelp_results = data.frame(fromJSON(results))
    names(yelp_results) = str_remove(list(names(yelp_results))[[1]],"businesses.")
    yelp_results$link = paste0("<a href='",yelp_results$url,"'>",yelp_results$name,"</a>")
    yelp_results$pic = paste("<img src=",yelp_results$image_url,"height='100'></img>")
    
    return(yelp_results  %>% select(link,pic,rating,price))
  },escape=FALSE)
  
  #------------------------------------------------------------------------------------------------------------------------
  
  output$tweets <- renderDataTable({
    rt <- get_trends(
      paste(input$destination))
    twit = rt %>% select(trend,url,tweet_volume)
    twit$link = paste0("<a href='",twit$url,"'>",twit$trend,"</a>")
    return(twit %>% select(link))
    
  },escape=FALSE)
  
  
  output$news <- renderDataTable({
    search_guardian <- function(text, page = 1) {
      r <- GET(
        "https://content.guardianapis.com/search",
        query = list(
          `api-key` = news_key,
          q = text,
          page = page
        )
      )
      stop_for_status(r)
      json <- content(r, as = "text", encoding = "UTF-8")
      fromJSON(json)$response
    }
    
    guard = search_guardian(paste(input$destination), 2)$results
    guard$webUrl = paste0("<a href='",guard$webUrl,"'>",guard$webTitle,"</a>")
    
    response <- guard %>% select(webUrl)
    
    return(response)
    
  },escape=FALSE)
  
}


shinyApp(ui = ui, server = server)
