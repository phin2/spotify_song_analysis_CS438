library(shinydashboard)
source("./spotify_func.R")
source("./model_make.R")

ui <- dashboardPage(
  dashboardHeader(title ="Spotify Music Reccomendation and Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Playlist", tabName = "playlist", icon = icon("music")),
      menuItem("User", tabName = "user", icon = icon("user"))
    )
  ),
  dashboardBody(
    #Contents of first tab
    tabItem(tabName = "playlist",
            demo_text <- 
              textInput("playlist_id", 
                        label = "Playlist", 
                        value = "", 
                        width = "100%",
                        placeholder = "Enter your spotify playlist uri")
    ),    submitButton("Get Reccomendation"),
    
    
    dataTableOutput(outputId = "tbl"),
    tabItem(tabName = "user")
  )
)

server <- function(input,output) {
  output$value <- renderText({input$playlist_id})
  getSongs <- reactive({
      songs <- pl_features(input$playlist_id)
  })
  output$tbl <- renderDataTable({getSongs()})

}

shinyApp(ui,server)