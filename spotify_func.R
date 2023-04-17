is_spotifyr_available <- require(spotifyr)
if (!is_spotifyr_available) {
  install.packages("spotifyr")
}
library(spotifyr)
#This file contains functions that use the request and send data
#from the Spotify API


#generates an access token
init_tkn <- function() {
  Sys.setenv(SPOTIFY_CLIENT_ID = 'cf21278a8c724825a166e40e82de9577')
  Sys.setenv(SPOTIFY_CLIENT_SECRET = 'a303f71e5bf04257b492b7b55d94a73d')
  
  access_token <- get_spotify_access_token()
}

#extracts features from a spotify playlist and stores them in a dataframe
pl_features <- function(pl_id,master) {
  
  access_token <- init_tkn()
  
  #gets songs from playlist
  pl <- get_playlist_audio_features("spotify",pl_id,access_token)[,c(1,6:17,19,31,36,37,45)]
  master <- rbind(master,pl)
  master[!duplicated(master),]
  
}

#gets a users stats from their id
user_pl_features <- function(user_id,master) {
  
  access_token <- init_tkn()
}
