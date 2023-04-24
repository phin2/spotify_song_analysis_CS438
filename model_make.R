
library(Rtsne)
library(readr)
library(ClusterR)
library(factoextra)
library(ggpubr)
library(cluster)
library(ggplot2)
library(caret)
library(proxy)
source("./spotify_func.R")
library(doBy)
library(dplyr)


#This file creates a graphs and visualizations for the dataset
#as well as recommending songs
#NOTE: the previous python version and the current R version of the
#data processing scripts produces data in different form so this script
#only works with the python version of the dataset

full_songs <- read_csv("csv_files/full_songs.csv", 
                       col_types = cols(`Unnamed: 0.1` = col_skip(), 
                                        `Unnamed: 0` = col_skip()))

data <- full_songs[,5:16]
data <- na.exclude(data)

norm <- preProcess(data,method = c("range"))
data <- predict(norm, data)

song_matrix <- as.matrix(data)

#k-means model for song recommendation
song.kmeans <- kmeans(song_matrix,9,algorithm = "Lloyd",iter.max = 1000)

#function that reccomends a song based on a playlist
rec <- function(playlist,df) {
  #normalizing the data
  norm <- preProcess(df[,5:16],method = c("range"))
  df[,5:16] <- predict(norm, df[,5:16])
  
  #getting songs from playlist
  pl <- pl_features(playlist)
  pl <- predict(norm,pl)
  #calculate average features for songs in playlist
  pl_score = colMeans(pl,na.rm = TRUE)
  pl_score = t(as.matrix(pl_score))

  #normalize playlist features
  norm_pl_score <-predict(norm,pl_score)
  norm_pl_score <- norm_pl_score[,c(7,1,2,8,3,9,4,5,6,11,12,10)]
  norm_pl_score <- t(as.matrix(norm_pl_score))

  #classify the playlist into a cluster
  pl_label <- which.min(dist(norm_pl_score,song.kmeans$centers))

  #get all playlist within cluster
  same_cluster <- full_songs[song.kmeans$cluster == pl_label,]
  dist = which.minn(dist(same_cluster[,5:16],norm_pl_score),5)
  
  
  recs <-full_songs[dist,c(2,3,18)]
  recs$track_uri<-gsub("spotify:track:","",as.character(recs$track_uri))
  
  rec_imgs <- song_features(recs[,3])
  recs[,3] = rec_imgs
  recs <-recs[,c(3,1,2)]
}

