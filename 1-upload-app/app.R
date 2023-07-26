
library(shiny)
library(tidyverse)
library(jsonlite)
library(purrr)
source("service.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Demo of reading a facebook file"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          "Instructions"
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fileInput("upload_deleted", "Upload a facebook file",
                    accept = ".zip"),
          # tableOutput("table_deleted")
          htmlOutput("html_deleted")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

#   fb_df <- unzip("facebook-vvsuschevskiy (1).zip", "friends_and_followers/removed_friends.json") |> read_json()
# 
# 
# fb_df |> 
#   as_tibble() |> 
#   unnest_wider(col = deleted_friends_v2) |> 
#   mutate(timestamp = timestamp |> as.Date.POSIXct())

  observeEvent(input$upload_deleted, {
    
    fb_df <- unzip(zipfile = input$upload_deleted$datapath,
                   files  = "friends_and_followers/removed_friends.json",
                   exdir = tempdir()
                   ) |> 
      read_json()
    
    
    fb_df <- fb_df |> 
      as_tibble() |> 
      unnest_wider(col = deleted_friends_v2) |> 
      mutate(timestamp = timestamp |> 
               as.Date.POSIXct()) |> 
      head(10) |> 
      mutate(id = row_number())
    
    if (nrow(fb_df) == 0) {
      # fb_df <- tibble(Error = "Sorry, there are no deleted friends")
      fb_html <- HTML(h4("Sorry, there are no deleted friends"))
    }

    # output$table_deleted <- renderTable(fb_df)
    output$html_deleted <- renderUI(fb_df |>
                                      purrr::pmap(data.frame) |>
                                      purrr::map(render_question))
  })
  
  onStop(function() {
    print("session ended")
    # unlink("friends_and_followers/removed_friends.json")
  })
    # # print("session ended"),
    # unlink("friends_and_followers"))

}

# Run the application 
shinyApp(ui = ui, server = server)
