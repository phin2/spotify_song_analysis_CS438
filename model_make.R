model_package_list <- c("Rtsne","readr","ClusterR","factoextra","ggpubr","cluster","ggplot2","caret","proxy","spotifyr","doBy","dplyr")
new_model_packages <- model_package_list[!(model_package_list %in% installed.packages()[,"Package"])]
if (length(new_model_packages) > 0) {install.packages(new_model_packages)}

library(Rtsne)
library(readr)
library(ClusterR)
library(factoextra)
library(ggpubr)
library(cluster)
library(ggplot2)
library(caret)
library(proxy)
library(doBy)
library(dplyr)

source("./spotify_func.R")

#This file creates a graphs and visualizations for the dataset
#as well as recommending songs
#NOTE: the previous python version and the current R version of the
#data processing scripts produces data in different form so this script
#only works with the python version of the dataset

full_songs <- read_csv("csv_files/full_songs.csv")
full_songs
data <- full_songs[,
                   #c(1:11,13)
                   5:16]
data <- na.exclude(data)

norm <- preProcess(data,method = c("range"))
data <- predict(norm, data)

song_matrix <- as.matrix(data)

#k-means model for song recommendation
song.kmeans <- kmeans(song_matrix,9,algorithm = "Lloyd",iter.max = 1000)

#function that reccomends a song based on a playlist
rec <- function(playlist,df,session) {
  
  playlist <- gsub("spotify:track:","",as.character(playlist))
  #normalizing the data
  norm <- preProcess(df[,5:16],method = c("range"))
  df[,5:16] <- predict(norm, df[,5:16])

  #getting songs from playlist
  pl <- pl_features(playlist)
  pl <- predict(norm,pl)
  #calculate average features for songs in playlist
  pl_score = colMeans(pl,na.rm = TRUE)
  pl_score <- t(as.data.frame(pl_score))

  #classify the playlist into a cluster
  pl_label <- which.min(dist(pl_score,song.kmeans$centers))
  #get all playlist within cluster
  same_cluster <- full_songs[song.kmeans$cluster == pl_label,]
  dist = which.minn(dist(same_cluster[,5:16],pl_score),5)
  
  
  recs <-full_songs[dist,c(2,3,18)]
  recs$track_uri<-gsub("spotify:track:","",as.character(recs$track_uri))
  
  rec_imgs <- song_features(recs[,3])
  recs[,3] = rec_imgs
  recs <-recs[,c(3,1,2)]
}

