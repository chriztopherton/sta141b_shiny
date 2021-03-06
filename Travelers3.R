
library(shiny)
library(dplyr)
library(shinythemes)
library(leaflet)


library(ggplot2)
library(tidyverse)
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
library(reactable)
library(httpuv)
library(stringr)
library(glue)
#----------------------------------------------------------------------------------------------------
#register for a free google maps api key at https://cloud.google.com/maps-platform/pricing
maps_key = Sys.getenv("maps_key")
register_google(key = maps_key)

news_key = Sys.getenv("news_key")

yelp_client_ID = Sys.getenv("yelp_client_ID")
yelp_key = Sys.getenv("yelp_key")

news_key = Sys.getenv("news_key")
#---------------------------------------------------------------------------------------------------------


ui <- fluidPage(theme=shinytheme("cyborg"),
                
                titlePanel(h1("TRAVELER'S GUIDE",align="center",style='background-color:teal;
                     padding-left: 15px',tags$img(height = 50,
                                                  width=70,
                                                  src ="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f0/Icon-notepad.svg/1024px-Icon-notepad.svg.png"))),
                #navbar Menu
                navbarPage("Plan It!",tabPanel("Itinerary",fluidPage(
                  
                  
                  
                 
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
                               searchInput # user input for Point A
                               (
                                 inputId = "origin", h4("Enter:",
                                                        tags$img(height = 50,
                                                                 width=50,
                                                                 src ="https://image.flaticon.com/icons/svg/1531/1531126.svg")),
                                 value = "Davis",btnSearch = icon("search"),
                                 btnReset = icon("remove"),width = "450px"
                               )),
                        
                        column(6,
                               searchInput #user input for Point B
                               (
                                 inputId = "destination",label = h4("Enter:",
                                                                    tags$img(height = 50,
                                                                             width=50,
                                                                             src ="https://www.iconbunny.com/icons/media/catalog/product/2/6/2657.9-finish-icon-iconbunny.jpg")),
                                 value = "Sacramento",btnSearch = icon("search"),
                                 btnReset = icon("remove"),width = "450px"
                               ))),
                      h5(textOutput("currentTime")), #current date and time
                      fluidRow(box(tableOutput(outputId = "dist_dur"))), #distancea and duration matrix
                      fluidRow(box(tableOutput(outputId = "weather"))), #OpenWeather API, only shows description and feels_like
                      
                      submitButton("Update")
                      
                      
                      
                    ),
                    
                    #----------------------------------------------------------------------------------------------------
                    
                    #main panel only includes google maps
                    mainPanel
                    (
                      leafletOutput(outputId = "map",
                                    width="100%",
                                    height = "630")
                      
                      
                    )),
                  
                  # below sidebarlayout are two columns, yelp and news
                  br(),
                  column(6,
                         wellPanel(h4("Yelp",align="center",tags$img(height = 50,
                                                                     width=50,
                                                                     src ="https://upload.wikimedia.org/wikipedia/commons/thumb/e/e8/Yelp.svg/1024px-Yelp.svg.png")),
                                   style = "padding: 5px;"),
                         
                         
                         wellPanel(dataTableOutput(outputId = "places"),style = "overflow-y:scroll; max-height: 520px"
                         )),
                  
                  
                  #Twitter could not publish along with Shiny app, commented out for now
                  
                  
                  # column(4,
                  #   wellPanel(h4("Twitter",align="center",tags$img(height = 50,
                  #                                  width=50,
                  #                                  src ="https://cdn2.iconfinder.com/data/icons/social-media-2285/512/1_Twitter_colored_svg-512.png")),
                  #             style = "padding: 5px;"),
                  #   
                  #            wellPanel(reactableOutput(outputId = "tweets"),style = "overflow-y:scroll; max-height: 670px"
                  # )),
                  
                  #new api
                  column(6,
                         wellPanel(h4("News",align="center",tags$img(height = 50,
                                                                     width=50,
                                                                     src ="https://img.favpng.com/20/25/11/newspaper-computer-icons-symbol-png-favpng-uxcxrxULJwf1TaT4zWQDsFUcw.jpg")),
                                   style = "padding: 5px;"),
                         wellPanel(dataTableOutput(outputId = "news"),style = "overflow-y:scroll; max-height: 800px")
                  ))
                ),
                
                #About section, includes description, contributors and references
                tabPanel("About",
                         wellPanel(
                           h3(strong("GOING TO SOMEPLACE NEW AND EXCITING?")),
                           h4("Well...this is your ONE STOP SHOP for finding out more!"),
                           p(strong("How to Navigate Traveler's Guide:")),
                           p("We make use of 4 different API's ( Google Maps, Weather, Yelp, News) to provide you with the complete picture
                           of the culture, events, and all things unique to your place of interest. Start by entering a starting location
                           as well as end ending and you'll be presented with an array of food places, conversations taking place, and
                           recent news. Not sure how to dress appropriately? You'll find weather information right below the selection boxes and 
                           routes and directions in the main panel to get you there on time. Let's go have some fun!"),
                           
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
                           #p("Twitter API, using rtweet: ", tags$a(href = "https://rtweet.info/", "rtweet")),
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
  
  
  #OpenWeather API, scrapes for weather description and feels like
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
                                            #main.temp_min,     #other optional features to be displayed
                                            #main.temp_max,
                                            #main.pressure,
                                            #main.humidity,
                                            #wind.speed)
    
  })
  #------------------------------------------------------------------------------------------------------------------------
  
  #reactive variables that take in the users input destination, gets route and directions
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
             addTiles() %>% addMarkers(lng = p1[2],lat = p1[1],  #markers for both origin and destination
                                       label = paste(input$origin),
                                       labelOptions = labelOptions(noHide = T,
                                                                   textsize = "15px")) %>%
             addMarkers(lng = p2[2],lat = p2[1],
                        label = paste(input$destination),
                        labelOptions = labelOptions(noHide = T,
                                                    textsize = "15px")))
  })
  
  
  
  #------------------------------------------------------------------------------------------------------------------------
  
  #yelp api scraper, gets the place, page and image url, urls are clickable
  
  yelp_results <- reactive({
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
    
    fromJSON(results)$businesses %>% 
      select(name,image_url,url,review_count,categories,rating,display_phone,price) %>% 
      mutate(link = paste0("<a href='",url,"'>",name,"</a>"),
             pic = paste("<img src=",image_url,"height='150'></img>"))
  })
  
  
  output$places <- renderDataTable({
    return(yelp_results() %>% select(Place = link, Price = price, Rating = rating, Img = pic))
  },options = list(searching = FALSE),escape=FALSE)
  
  #------------------------------------------------------------------------------------------------------------------------
  
  
  #commented out for now, couldnt get to work, perhaps in the future
  
  # twit <- reactive({
  #   get_trends(paste(input$destination))  %>% 
  #     select(trend,url) %>% 
  #     mutate(link = paste0("<a href='",url,"'>",trend,"</a>")) %>% 
  #     select(link)
  # })
  # 
  # output$tweets <- renderReactable({
  #   
  #   reactable::reactable(twit(),
  #             filterable = TRUE, searchable = FALSE, bordered = TRUE, striped = TRUE, highlight = TRUE,
  #             showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
  #             columns = list(
  #              link = colDef(html=TRUE)
  #             ))
  # 
  # })
  
  #------------------------------------------------------------------------------------------------------------------------
 
  #the guardian new api, gets the headline and its category
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
    
  },options = list(searching = FALSE),escape=FALSE)
  
}

#------------------------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)