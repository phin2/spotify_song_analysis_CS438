shiny_package_list <- c("shiny" ,"shinydashboard","shinycssloaders")
new_shiny_packages <- shiny_package_list[!(shiny_package_list %in% installed.packages()[,"Package"])]
if (length(new_shiny_packages) > 0) {install.packages(new_shiny_packages)}
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(ggvis)
library(dplyr)

source("./spotify_func.R")
source("./model_make.R")

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Recommend", tabName = "playlist", icon = icon("music")),
    menuItem("Explore", tabName = "explore", icon = icon("magnifying-glass"))
  ))

body <- dashboardBody(
  #Contents of first tab
  tabItems(
    tabItem(tabName = "playlist",
            textInput("playlist_id", 
                      label = "Playlist", 
                      value = "", 
                      width = "100%",
                      placeholder = "Enter your spotify playlist uri"),
            actionButton("recommend","Get Recommendation"),
            withSpinner(DT::dataTableOutput(outputId = "tbl"),type=2)
    ),
    tabItem(tabName = "explore"

    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title ="Spotify Music Reccomendation and Analysis"),
  sidebar,
  body
)

server <- function(input,output) {
  songs <- data.frame()
  shinyInput = function(FUN, len, id, ...) { 
    inputs = character(len) 
    for (i in seq_len(len)) { 
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...)) 
    } 
    inputs 
  }
  output$value <- renderText({input$playlist_id})
  getSongs <- eventReactive(input$recommend, {
    songs <- rec(input$playlist_id,full_songs)
  })

  output$tbl <- DT::renderDataTable({
    DT::datatable(getSongs(),escape=FALSE)
    })

}

shinyApp(ui=ui,server=server)