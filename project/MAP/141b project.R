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
#----------------------------------------------------------------------------------------------------
#register for a free google maps api key at https://cloud.google.com/maps-platform/pricing
api_key = "AIzaSyAYkxy_nMYU1aopQ6Ec5rIhClQbYny846Y"
register_google(key = api_key)
#---------------------------------------------------------------------------------------------------------


ui <- fluidPage(theme=shinytheme("spacelab"),
                
                titlePanel("Google Maps API"),
                
                
                # user input for Point A
                sidebarPanel
                (
                    selectInput
                    (
                        inputId = "origin", 
                        label = strong("Start:"),
                        choices = c("UCD Pavillion", 
                                    "Golden 1 Center", 
                                    "Tahoe City"),
                        selected = "UCD Pavillion"
                    ),
                    
                    # user input for Point B
                    selectInput
                    (
                        inputId = "destination", 
                        label = strong("End:"),
                        choices = c("UCD Pavillion", 
                                    "Golden 1 Center", 
                                    "Tahoe City"),
                        selected = "Golden 1 Center"
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
                                               height = "800")),
                        tabPanel("Data",
                                 tableOutput(outputId = "distancetable")),
                        tabPanel("Tweets/Info/Food/News/")
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
                                  key = paste(api_key))
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

    # output$map <- renderLeaflet({
    #     req(doc())
    # 
    #     p1 = c(geocode(paste(input$origin))$lat,geocode(paste(input$origin))$lon)
    #     p2 = c(geocode(paste(input$destination))$lat,geocode(paste(input$destination))$lon)
    #     coord = data.frame(p1,p2)
    # 
    #     
    #     leaflet(seg()) %>%
    #         addPolylines(opacity = 3,
    #                      weight = 7,
    #                      color = ~pal()(seg()$segment_id),
    #                      popup = ~seg()$instructions) %>%
    #         addTiles()
    # })
    
    #distance matrix
    output$distancetable <- renderTable({
        
        locations = c(input$origin,input$destination)
        dist = mp_matrix(origins= locations,
                         destinations = locations,
                         key = paste(api_key))

        m1 = mp_get_matrix(dist, value = "distance_text")
        colnames(m1) = locations
        rownames(m1) = locations

        m2 = mp_get_matrix(dist, value = "duration_text")
        colnames(m1) = locations
        rownames(m1) = locations

        if(input$show_distance){
            DT::datatable(data = m1,rownames = FALSE)
            m2
         }
        
        
    })
    
}


shinyApp(ui = ui, server = server)
