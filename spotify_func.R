install.packages('spotifyr')
library(spotifyr)
library(dplyr)
#This file contains functinos that use the request and send data
#from the spotify api 


#generates an access token
init_tkn <- function() {
  Sys.setenv(SPOTIFY_CLIENT_ID = 'cf21278a8c724825a166e40e82de9577')
  Sys.setenv(SPOTIFY_CLIENT_SECRET = 'a303f71e5bf04257b492b7b55d94a73d')
  
  access_token <- get_spotify_access_token()
}

#extracts features from a spotify playlist and stores them in a dataframe
pl_features <- function(pl_id) {
  
  access_token <- init_tkn()
  
  #gets songs from playlist
  pl <- get_playlist_audio_features("spotify",pl_id,access_token)[,c(6:16,19)]

}

#gets a users stats from their id
user_pl_features <- function(user_id,master) {
  
  access_token <- init_tkn()
}

song_features <-function(track_id) {
    access_token <- init_tkn()
    songs <- apply(track_id,1,get_tracks)
    imgs <- bind_rows(songs,.id="column_label")
    imgs <- bind_rows(imgs$album.images,.id ="column_label")
    imgs <- imgs[!duplicated(imgs[,1]),]$url
    imgs <- paste('<img src="',imgs,'" height = "50"></img>',sep="")

}

  