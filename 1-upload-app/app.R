#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(jsonlite)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Demo of reading a facebook file"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          "Instructions"
        #     sliderInput("bins",
        #                 "Number of bins:",
        #                 min = 1,
        #                 max = 50,
        #                 value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fileInput("upload_deleted", "Upload a facebook file"),
          tableOutput("table_deleted")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

#   fb_df <- unzip("facebook-vvsuschevskiy (1).zip", "friends_and_followers/removed_friends.json") |> read_json()
# 
# 
# fb_df |> 
#   as_tibble() |> 
#   unnest_wider(col = deleted_friends_v2) |> 
#   mutate(timestamp = timestamp |> as.Date.POSIXct())

  observeEvent(input$upload_deleted, {
    
    fb_df <- unzip(input$upload_deleted$datapath, "friends_and_followers/removed_friends.json") |> 
      read_json()
    
    
    fb_df <- fb_df |> 
      as_tibble() |> 
      unnest_wider(col = deleted_friends_v2) |> 
      mutate(timestamp = timestamp |> as.Date.POSIXct()) |> 
      head(10)
    
    if (nrow(fb_df) == 0) {
      fb_df <- tibble(Error = "Sorry, there are no deleted friedns")
    }
    
    output$table_deleted <- renderTable(fb_df)
    
  })
  
  # output$files <- renderTable(input$upload)
}

# Run the application 
shinyApp(ui = ui, server = server)
