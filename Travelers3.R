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


ui <- fluidPage(theme=shinytheme("cyborg"),
                
                titlePanel(h1("TRAVELER'S GUIDE",align="center",style='background-color:teal;
                     padding-left: 15px',tags$img(height = 50,
                                                  width=70,
                                                  src ="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f0/Icon-notepad.svg/1024px-Icon-notepad.svg.png"))),
                navbarPage("Plan It!",tabPanel("Itinerary",fluidPage(
                
    
                                    
                                    # user input for Point A
                fluidRow(
                                    sidebarPanel
                                    (
                                      h3("Explore",align="center",tags$img(height = 200,
                                                   width=250,
                                                   #src = "https://starecat.com/content/wp-content/uploads/pikachu-pokemon-people-with-fever-resting-in-bed-people-with-coronavirus-i-will-travel-across-the-land.jpg")),
                                                   src = "https://www.moneycrashers.com/wp-content/uploads/2019/04/plan-road-trip-tips-ideas.jpg")),
                                      hr(),
                                      
                                      fluidRow(
                                        column(6,
                                      searchInput
                                      (
                                        inputId = "origin", h4("Enter:",
                                                               tags$img(height = 50,
                                                                        width=50,
                                                                        src ="https://image.flaticon.com/icons/svg/1531/1531126.svg")),
                                        value = "Davis",btnSearch = icon("search"),
                                        btnReset = icon("remove"),width = "450px"
                                      )),
                                      
                                      column(6,
                                      searchInput
                                      (
                                        inputId = "destination",label = h4("Enter:",
                                                                           tags$img(height = 50,
                                                                                    width=50,
                                                                                    src ="https://www.iconbunny.com/icons/media/catalog/product/2/6/2657.9-finish-icon-iconbunny.jpg")),
                                        value = "Sacramento",btnSearch = icon("search"),
                                        btnReset = icon("remove"),width = "450px"
                                      ))),
                                      h5(textOutput("currentTime")),
                                      fluidRow(box(tableOutput(outputId = "dist_dur"))),
                                      fluidRow(box(tableOutput(outputId = "weather"))),
                                      
                                     submitButton("Update!")

                                  
                                      
                                    ),
                                    
                                    #----------------------------------------------------------------------------------------------------
                                    mainPanel
                                    (
                                      #box(width = 6,h4("Google Maps",align="center",tags$img(height = 100,width=220,src ="https://www.onlinemarketingwhiz.com.au/wp-content/uploads/2018/07/google-maps-ios-icon-top.png")),style = "padding: 5px;"),
                                      #tableOutput(outputId = "weather"),
                                      leafletOutput(outputId = "map",
                                                    width="100%",
                                                    height = "630")
                                
                                             
                                    )),
                                     br(),
                                      column(4,
                                        wellPanel(h4("Yelp",align="center",tags$img(height = 50,
                                                           width=50,
                                                           src ="https://upload.wikimedia.org/wikipedia/commons/thumb/e/e8/Yelp.svg/1024px-Yelp.svg.png")),
                                                  style = "padding: 5px;"),
                                                 
                                        
                                        wellPanel(dataTableOutput(outputId = "places"),style = "overflow-y:scroll; max-height: 670px"
                                      )),
                                    
                
                                      column(4,
                                        wellPanel(h4("Twitter",align="center",tags$img(height = 50,
                                                                       width=50,
                                                                       src ="https://cdn2.iconfinder.com/data/icons/social-media-2285/512/1_Twitter_colored_svg-512.png")),
                                                  style = "padding: 5px;"),
                                        
                                                 wellPanel(dataTableOutput(outputId = "tweets"),style = "overflow-y:scroll; max-height: 670px"
                                      )),
                                     
                                      column(4,
                                        wellPanel(h4("News",align="center",tags$img(height = 50,
                                                           width=50,
                                                           src ="https://img.favpng.com/20/25/11/newspaper-computer-icons-symbol-png-favpng-uxcxrxULJwf1TaT4zWQDsFUcw.jpg")),
                                                  style = "padding: 5px;"),
                                        wellPanel(dataTableOutput(outputId = "news"),style = "overflow-y:scroll; max-height: 800px")
                                        ))
                ),
                tabPanel("About",
                         wellPanel(
                         h3(strong("GOING TO SOMEPLACE NEW AND EXCITING?")),
                         h4("Well...this is your ONE STOP SHOP for finding out more!"),
                         p(strong("How to Navigate Traveler's Guide:")),
                         p("We make use of 4 different API's ( Google Maps, Weather, Yelp, Twitter, News) to provide you with the complete picture
                           of the culture, events, and all things unique to your place of interest. Start by entering a starting location
                           as well as end ending and you'll be presented with an array of food places, conversations taking place, and
                           recent news. Not sure how to dress appropriately? You'll find weather information right below and routes and directions 
                           to get you there on time. Let's go have some fun!"),
        
                         hr()),
                         wellPanel(
                         h4(strong("Contributors")),
                         tags$ul(
                           tags$li("Christopher Ton: Front and Back end, User Documenation "),
                           tags$li("Jesus Leon: News API, User Documentation "),
                           tags$li("Xuecheng Zhang: Yelp API, User Documentation "),
                           tags$li("Yinglin Luo: User Documenation ")
                         ),
                         hr()),
                         wellPanel(
                         h4(strong("References")),
                         p("Google Maps API, using R package mapsapi: ", tags$a(href = "https://cran.rstudio.com/web/packages/mapsapi/vignettes/intro.html", "Introduction to package mapsapi")),
                         p("Weather API:", tags$a(href = "https://openweathermap.org/", "OpenWeather")),
                         p("Yelp API:", tags$a(href = "https://https://www.yelp.com/fusion/", "Yelp Fusion")),
                         p("Twitter API, using rtweet: ", tags$a(href = "https://rtweet.info/", "rtweet")),
                         p("News API: ", tags$a(href = "https://open-platform.theguardian.com/", "TheGuardianOpenPlatform")),
                         h4("Please visit",tags$a(href = "https://github.com/chriztopherton/sta141b_shiny", "sta141b_shiny"), "to see our project. Thanks for visiting!!"),
                         hr()
                         ))
                ,position = "static-top",collapsible = TRUE)
)


#------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
  
  output$weather = renderTable({
    r = GET(
      "https://api.openweathermap.org/data/2.5/weather",
      query = list(
        appid = Sys.getenv("weather_key"),
        lat = geocode(paste(input$destination))$lat,
        lon = geocode(paste(input$destination))$lon
      ))
    r
    
    stop_for_status(r)
    
    json <- content(r, as = "text")
    data.frame(fromJSON(json)) %>% select(weather.description,
                                          main.feels_like)
                                          #main.temp_min,
                                          #main.temp_max,
                                          #main.pressure,
                                          #main.humidity,
                                          #wind.speed)
    
  })
  #------------------------------------------------------------------------------------------------------------------------
  doc = reactive({mp_directions(origin = input$origin, #from UCD Pav
                                destination = input$destination,
                                mode = c("driving", "transit", "walking", "bicycling"),
                                alternatives = FALSE,
                                waypoints = NULL,
                                key = paste(maps_key))
  })
  
  r = reactive({mp_get_routes(doc())})
  
  output$dist_dur <- renderTable({
    c = data.frame(c("Distance",r()$distance_text),c("Duration",r()$duration_text))
    names(c) = c("","")
    c
  },options = list(searching = FALSE))
  
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
    yelp_results$pic = paste("<img src=",yelp_results$image_url,"height='150'></img>")
    
    return(yelp_results %>% select(link,pic,rating,price))
  },options = list(searching = FALSE),escape=FALSE)
  
  #------------------------------------------------------------------------------------------------------------------------
  
  output$tweets <- renderDataTable({
    rt <- get_trends(
      paste(input$destination))
    twit = rt %>% select(trend,url,tweet_volume)
    twit$link = paste0("<a href='",twit$url,"'>",twit$trend,"</a>")
    return(twit %>% select(link))
    
  },options = list(searching = FALSE),escape=FALSE)
  
  #------------------------------------------------------------------------------------------------------------------------
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

#------------------------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

