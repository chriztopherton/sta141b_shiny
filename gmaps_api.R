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

#---------------------------------------------------------------------------------------------------------


ui <- fluidPage(theme=shinytheme("spacelab"),
                
                titlePanel("Google Maps API"),
    
                  
                  # user input for Point A
                  sidebarPanel
                  (
                    # selectInput
                    # (
                    #   inputId = "origin", 
                    #   label = strong("Start:"),
                    #   choices = c("UCD Pavillion", 
                    #               "Golden 1 Center", 
                    #               "Tahoe City"),
                    #   selected = "UCD Pavillion"
                    # ),
                    
                    searchInput
                    (
                      inputId = "origin",label = strong("Start:"),btnSearch = icon("search"),
                      btnReset = icon("remove"),width = "450px"
                    ),
                    
                    # user input for Point B
                    # selectInput
                    # (
                    #   inputId = "destination", 
                    #   label = strong("End:"),
                    #   choices = c("UCD Pavillion", 
                    #               "Golden 1 Center", 
                    #               "Tahoe City"),
                    #   selected = "Golden 1 Center"
                    # ),
                    
                    searchInput
                    (
                      inputId = "destination",label = strong("End:"),btnSearch = icon("search"),
                      btnReset = icon("remove"),width = "450px"
                    ),
                    
                    #whether or not distance matrix is shown
                    checkboxInput
                    (
                      inputId = "show_distance",
                      label = strong("Show Distance"),
                      value = TRUE)
                  ),
                  
                  #----------------------------------------------------------------------------------------------------
                  mainPanel
                  (
                    tabsetPanel
                    (
                      tabPanel("Google Maps",
                               leafletOutput(outputId = "map",
                                             width="100%",
                                             height = "800"),
                               tableOutput(outputId = "distancetable")),
                      tabPanel("Yelp",DT::dataTableOutput(outputId = "places")),
                      tabPanel("Twitter",DT::dataTableOutput(outputId = "tweets"))
                    )
                    )
                  )
                
#------------------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
  
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
  #print(r)
  
  seg = reactive({mp_get_segments(doc())})
  #print(seg)
  
  #plots coordinates and directions
  pal = reactive({colorFactor(palette = sample(colors(), length(unique(seg()$segment_id))),domain = seg()$segment_id)})
  
  output$map <- renderLeaflet({
    req(doc())

    p1 = c(geocode(paste(input$origin))$lat,geocode(paste(input$origin))$lon)
    p2 = c(geocode(paste(input$destination))$lat,geocode(paste(input$destination))$lon)
    coord = data.frame(p1,p2)


    leaflet(seg()) %>%
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
                                             textsize = "15px"))
      #addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me",onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  #distance matrix
  output$distancetable <- renderTable({
     
    locations = c(input$origin,input$destination)
    # dist = mp_matrix(origins= locations, 
    #                   destinations = locations,
    #                   key = paste(maps_key))
    #  
    # m1 = mp_get_matrix(dist, value = "distance_text") 
    #  colnames(m1) = locations 
    #  rownames(m1) = locations 
    # 
    #  m2 = mp_get_matrix(dist, value = "duration_text") 
    #  colnames(m1) = locations 
    #  rownames(m1) = locations 
    #  
    #  if(input$show_distance){
    #    DT::datatable(data = m1,rownames = FALSE) 
    #    m2
    #    }
})
  
  #------------------------------------------------------------------------------------------------------------------------
  
  
  output$places <- DT::renderDataTable({
    
    
    
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
    
    DT::datatable(data =yelp_results %>% select(name,rating) %>% arrange(desc(rating)))
  })
  
  #------------------------------------------------------------------------------------------------------------------------
  
  output$tweets <- DT::renderDataTable({
    rt <- get_trends(
      "sacramento")
    
    DT::datatable(data =rt )
    
    
  })
}


shinyApp(ui = ui, server = server)

