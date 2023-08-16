
library(shiny)
library(tidyverse)
library(jsonlite)
library(purrr)
library(colorspace)
library(shinyjs)
library(digest)

source("service.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),

    # Application title
    titlePanel("Unfriending on Face"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          HTML("
               <b> Instructions and opt-out: </b> <br>
               In the following survey, we will ask questions about 
               up to eight of your most recently removed Facebook 
               connections, and up to eight of your current connections. 
               If there are any individuals that you wish to avoid 
               discussing, please uncheck their names here.
               ")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidPage(
          div(id = "upload_deleted_div",
              fileInput("upload_deleted", "Upload a facebook file",
                        accept = ".zip"),
              htmlOutput("html_confirm_friends")
              ),
          div(id = "questions_1_div",
              htmlOutput("html_deleted")
              ),
          div(id = "verification_div",
              htmlOutput("html_verification")
          )
        )
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  values <- reactiveValues(fb_df = NULL,
                           fb_df_filtered = NULL)
  
  # values$fb_df <- tibble(name = NULL)

  observeEvent(input$upload_deleted, {
    
    fb_df <- unzip(zipfile = input$upload_deleted$datapath,
                   files  = "friends_and_followers/removed_friends.json",
                   exdir = tempdir()
                   ) |> 
      read_json()
    
    
    values$fb_df <- fb_df |> 
      as_tibble() |> 
      unnest_wider(col = deleted_friends_v2) |> 
      mutate(timestamp = timestamp |> 
               as.Date.POSIXct()) |> 
      head(10) |> 
      mutate(id = row_number()) |> 
      mutate(bg_color = n() |> 
               purple_colfunc() |> 
               lighten(amount = 0.75))
    
    if (nrow(values$fb_df) == 0) {
      output$html_deleted <- renderUI(
        HTML(
          h4("Sorry, there are no deleted friends")
          )
        )
    }
    
    
    output$html_confirm_friends <- renderUI(
      column(
        width = 8,
        checkboxGroupInput(
          inputId = "checkboxGroupInput_fb_cpnfirm",
          label = "Selected deleted friends",
          choices = values$fb_df$name,
          selected  = values$fb_df$name
        ),
        actionButton(inputId = "actionButton_fb_confirm",
                     label = "Confirm")
      )
    )
    
    
  })
  
  
  
  observeEvent(input$actionButton_fb_confirm, {
    hide("upload_deleted_div")
    
    values$fb_df_filtered <- isolate(values$fb_df) |> 
      filter(name %in% input$checkboxGroupInput_fb_cpnfirm)

    if(nrow(values$fb_df_filtered) < 1){
      ## TODO: message or something
    } else {
      output$html_deleted <- renderUI(
        column(
          width = 8,
        fluidRow(
        isolate(values$fb_df_filtered) |>
          purrr::pmap(data.frame) |>
          purrr::map(render_question),
      ),
      fluidRow(
        div(
          actionButton(inputId = "actionButton1_verify",
                       label = "Next"),
          style = "display:inline-block; float:right;"
        ),
      )
      ))
    }
    
  })
  
  observeEvent(input$actionButton1_verify, {
    hide("questions_1_div")
    
    
    output_df <- tibble(
      user = session$token,
      friend = 
    )
    
    output$html_verification <- renderUI(
      column(
        width = 10,
        renderTable(tibble(user = session$token,
                           fri) |> 
                      head(10)),
        actionButton(inputId = "actionButton_sumbit",
                   label = "Sumbit")
      )
    )
    
    
  })
  
  
  onStop(function() {
    print("session ended")
    # unlink("friends_and_followers/removed_friends.json")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
