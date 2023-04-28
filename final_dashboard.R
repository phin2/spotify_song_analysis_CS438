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

#all the possible values for our axis
axis_vars <- c(
  "Acousticness" = "acousticness",
  "Danceability" = "danceability",
  "Energy" = "energy",
  "Liveness" = "liveness",
  "Loudness" = "loudness",
  "Speechiness" = "speechiness",
  "Tempo" = "tempo",
  "Valence" = "valence",
  "Popularity" = "popularity",
  "Duration" = "duration_ms"
)

#loading bar
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Recommend", tabName = "playlist", icon = icon("music")),
    menuItem("Explore", tabName = "explore", icon = icon("magnifying-glass"))
  ))

body <- dashboardBody(
  tabItems(
    #Contents of first tab, song recommendation tab
    tabItem(tabName = "playlist",
            textInput("playlist_id", 
                      label = "Playlist", 
                      value = "", 
                      width = "100%",
                      placeholder = "Enter your spotify playlist uri"),
            actionButton("recommend","Get Recommendation"),
            withSpinner(DT::dataTableOutput(outputId = "tbl"),type=2)
    ),
    #Contents of the second tab, exploration of the dataset
    tabItem(tabName = "explore",
            fluidPage(
              sidebarLayout(
                sidebarPanel(
                  textInput("song_name","Name of the Song"),
                  textInput("artist","Name of the Artist"),
                  sliderInput("acousticness",label="Acousticness",min = 0,max=1,value=c(0,1)),
                  sliderInput("danceability",label="Danceability",min = 0,max=1,value=c(0,1)),
                  sliderInput("energy",label="Energy",min = 0,max=1,value=c(0,1)),
                  sliderInput("liveness",label="Liveness",min = 0,max=1,value=c(0,1)),
                  sliderInput("speechiness",label="Speechiness",min = 0,max=1,value=c(0,1)),
                  sliderInput("tempo",label="Tempo",min = 0,max=300,value=c(0,300)),
                  sliderInput("valence",label="Valence",min=0,max=1,value=c(0,1)),
                  selectInput("xvar","X-Axis Variable: ",axis_vars,selected="popularity"),
                  selectInput("yvar","Y-Axis Variable: ",axis_vars,selected="duration_ms")
                  
                ),
                mainPanel(
                  ggvisOutput("plot")
                )
                
              ))
            
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title ="Spotify Music Reccomendation and Analysis"),
  sidebar,
  body
)

server <- function(input,output,session) {
  songs <- data.frame()

  output$value <- renderText({input$playlist_id})
  
  #gets recommended songs from playlist id
  getSongs <- eventReactive(input$recommend, {
    songs <- rec(input$playlist_id,full_songs)
  })
  
  #data frame for displaying song recommendaions
  output$tbl <- DT::renderDataTable({
    DT::datatable(getSongs(),escape=FALSE)
  })
  
  
  
  #filters out songs that don't meet conditions from slider inputs
  
  #gets tracks based on current parameters
  tracks <- reactive({
    min_acoust <- input$acousticness[1]
    max_acoust <- input$acousticness[2]
    
    min_dance <- input$danceability[1]
    max_dance <- input$danceability[2]
    
    min_energy <- input$energy[1]
    max_energy <- input$energy[2]
    
    min_live <- input$liveness[1]
    max_live <- input$liveness[2]
    
    min_loud <- input$loudness[1]
    max_loud <- input$loudness[2]
    
    min_speech <- input$speechiness[1]
    max_speech <- input$speechiness[2]
    
    min_tempo <- input$tempo[1]
    max_tempo <- input$tempo[2]
    
    min_valence <- input$valence[1]
    max_valence <- input$valence[2]
    
    
    #filters tracks
    s <- full_songs %>% 
      filter(
        acousticness >= min_acoust,
        acousticness <= max_acoust,
        danceability >= min_dance,
        danceability <= max_dance,
        energy >= min_energy,
        energy <= max_energy,
        liveness >= min_live,
        liveness <= max_live,
        speechiness >= min_speech,
        speechiness <= max_speech,
        tempo >= min_tempo,
        tempo <= max_tempo,
        valence >= min_valence,
        valence <= max_valence
        
      )
    
    if(!is.null(input$artist) && input$artist != "") {
      s <- s %>% filter(grepl(toupper(input$artist),toupper(artist_name)))
    }
    
    if(!is.null(input$song_name) && input$song_name != "") {
      s <- s %>% filter(grepl(toupper(input$song_name),toupper(track_name)))
    }
    
    s <- as.data.frame(s)
  })
  
  #generates a tooltip for song that is currently hovered on
  song_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$track_uri)) return(NULL)
    # Pick out the movie with this ID
    songs <-isolate(tracks())
    print(x$track_uri)
    song <- songs[songs$track_uri == x$track_uri, ]
    
    paste0("<b>Song Name: ", song$track_name, 
           "</b><br> Artist: ",song$artist_name,
           "<br> Acousticness: ", as.character(song$acousticness),
           "<br> Danceability: ", as.character(song$danceability),
           "<br> Energy: ", as.character(song$energy),
           "<br> Speechiness: ", as.character(song$speechiness),
           "<br> Tempo: ", as.character(song$tempo),
           "<br> Valence: ", as.character(song$valence))
  }
  
  
  #Creates a scatter plot of songs popularity vs duration_ms
  output$plot <- reactive({
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    xvar <- prop("x",as.symbol(input$xvar))
    yvar <- prop("y",as.symbol(input$yvar))
    tracks %>%
      ggvis(x = xvar, y = yvar, key:= ~track_uri) %>%
      layer_points(size := 50, size.hover := 200,fillOpacity := 0.2, fillOpacity.hover := .5,fill.hover :="#1DB954") %>%
      add_axis("x",title = xvar_name) %>%
      add_tooltip(song_tooltip, "hover") %>%
      add_axis("y",title = yvar_name) %>%
      set_options(width = 500, height = 500) %>%
      bind_shiny("plot")
    
  })
  
}

shinyApp(ui=ui,server=server)