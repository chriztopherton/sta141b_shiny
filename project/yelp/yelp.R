#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(maps)
library(mapproj)
library(ggmap)
library(leaflet)
library(httr)
library(jsonlite)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("travel strategy"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel
        (
            p("start your trip plan"),
            hr(),
            textInput("search_box", "Type your location here"),
            textInput("location_box", "Type your destination here"),
            
            # filters for the map: price, 
            radioButtons("business_filter", label = "Prices", choices = list("No Preference" = "", "$", "$$", "$$$", "$$$$")),
            actionButton("location_button", label = "", icon = shiny::icon("search"))
        ),
        # selectInput
        # (
        #     inputId = "origin", 
        #     label = strong("Start:"),
        #     choices = c("UCD Pavillion", 
        #                 "Golden 1 Center", 
        #                 "Tahoe City"),
        #     selected = "UCD Pavillion"
        # ),
        # selectInput
        # (
        #     inputId = "destination", 
        #     label = strong("End:"),
        #     choices = c("UCD Pavillion", 
        #                 "Golden 1 Center", 
        #                 "Tahoe City"),
        #     selected = "Golden 1 Center"
        # ),
        # Show a plot of the generated distribution
        
        
        
        mainPanel(
            tabsetPanel
            (
                tabPanel("Google Maps",
                         leafletOutput(outputId = "map",
                                       width="100%",
                                       height = "800"),
                         tableOutput(outputId = "distancetable")),
                #outputs the map
                tabPanel("Yelp",leafletOutput("myMap",height = "800")),
                tabPanel("Twitter")
        )
    )
))












server <- function(input, output) {

    #yelp
    Sys.setenv(yelp_client_ID='b1mvtJ6NN1Yr3llYIn0jkw')
    Sys.setenv(yelp_key='jDwHiw2npYP2mjdD0vmcJW_PG3RDz7eA6gmLdPGWAObdQ_G2kBG4-5GE_VMpyCSRT_oWXdHV3u9HDwSPbL4qSDl9pnkgdFrD3ZASmcIB4NlUY320H-0jXtfGaOhdXnYx')
    
    
    yelp_client_ID = Sys.getenv("yelp_client_ID")
    yelp_key = Sys.getenv("yelp_key")
    
    #create token with provided id, key
    res <- POST("https://api.yelp.com/oauth2/token",
                body = list(grant_type = "client_credentials",
                            client_id = yelp_client_ID,
                            client_secret = yelp_key))
    getData <- function(query.params) {
        path = "businesses/search" 
        response <- GET(url = paste(base_yelp_url, path, sep = ""), query = query.params, add_headers('Authorization' = paste("bearer", yelp_api_key)), content_type_json())
        body <- content(response, "text")
        data <- fromJSON(body)
        return (data)
    }
    
    
    # creates a default map zoomed out to view the US 
    map <- leaflet() %>% addTiles() %>% setView(-101.204687, 40.607628, zoom = 3)
    output$myMap <- renderLeaflet(map)
    
    
    # preps variables that will be used later for plotting
    business_frame <- data.frame()
    center <- vector("list")
    
    
    # waits for the button to be pressed before getting data to be plotted
    observeEvent(input$location_button, {
        query.params = list(location = input$location_box)
        specific_data <- getData(query.params)
        
        # extracts the long, lat of the middle of the data set in question
        region <- specific_data[[3]]
        center <- region[[1]]
        
        # flattens and extracts into one data frame
        business_frame <- flatten(specific_data[[1]])
        
        # filters the resultant data based on an inputted price level
        if (input$business_filter != "") {
            business_frame <- filter(business_frame, price == input$business_filter)
        }
        
        # ensures that the map does not error out if the data frame is empty
        # if it is empty, the map will default to the long, lat of the region from the search box
        if (nrow(business_frame) == 0) {
            view_city <- geocode(input$location_box)
            output$myMap <- renderLeaflet(map %>% setView(view_city[[1]], view_city[[2]], zoom = 13))
        } else {
            output$myMap <- renderLeaflet(map %>% 
                                              setView(center[[1]],center[[2]], zoom = 13) %>% 
                                              addAwesomeMarkers(lng = business_frame$coordinates.longitude, 
                                                                lat = business_frame$coordinates.latitude, icon=icons, label = business_frame$name))
        }  
        
        # sets the color of the icons to be used  
        getColor <- function(business_frame) {
            sapply(business_frame$rating, function(rating) {
                if(rating >= 4.5) {
                    "green"
                } else if(rating >= 3.5) {
                    "orange"
                } else {
                    "red"
                } })
        }
        
        # creates a list of icons to be used by the map
        icons <- awesomeIcons(
            icon = 'ios-close',
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(business_frame)
        )
        
        
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
