model_package_list <- c("Rtsne","readr","ClusterR","factoextra","ggpubr","cluster","ggplot2","caret","proxy")
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

#This file creates a graphs and visualizations for the dataset
#as well as recommending songs
#NOTE: the previous python version and the current R version of the
#data processing scripts produces data in different form so this script
#only works with the python version of the dataset

#import song data
full_songs <- read_csv("./csv_files/full_songs.csv", 
                       col_types = cols(duration_ms = col_number(),acousticness = col_number(),
                                        danceability = col_number(),energy = col_number(),
                                        instrumentalness = col_number(),key = col_number(), 
                                        liveness = col_number(),loudness = col_number(),mode = col_number(),
                                        speechiness = col_number(), tempo = col_number(),
                                        time_signature = col_number(), valence = col_number(),
                                        popularity = col_number()))
data <- full_songs[,5:16]
data <- na.exclude(data)

norm <- preProcess(data,method = c("range"))
data <- predict(norm, data)

song_matrix <- as.matrix(data)

#dimensionality reduction on song data
# tsne_out <- Rtsne(data,check_duplicates=FALSE)
# tsne_plot <- data.frame(tsne_out$Y[,1],tsne_out$Y[,2])


#k-means model for song recommendation
song.kmeans <- kmeans(song_matrix,5,algorithm = "Lloyd",iter.max = 1000)

#finding optimal number of clusters
fviz_nbclust(data,kmeans,method="wss")

#silhouette plots
dis <- dist(song_matrix)^2
sil <- silhouette(song.kmeans$cluster,dis)
fviz_silhouette(sil,print.summary = TRUE)

# fviz_cluster(song.kmeans, data = tsne_plot,
#              geom = "point",
#              ellipse.type = "convex", 
#              ggtheme = theme_bw()
# )

# #Gaussian mixture model for song recommendation
# song.gmm <- GMM(data,7)
# mediod <- Cluster_Medoids(tsne_plot,7)
# gmm.pred <- predict_GMM(data, song.gmm$centroids,song.gmm$covariance_matrices,song.gmm$weights)
# plot_2d(tsne_plot,gmm.pred$cluster_labels,mediod$medoids)


#currently assumes that all songs in your playlist are in the database
#function that reccomends a song based on a playlist
recommend <- function(playlist,df) {
  #normalizing the data
  norm <- preProcess(df[,5:16],method = c("range"))
  df[,5:16] <- predict(norm, df[,5:16])

  pl = list(c())
  #search for songs in the database
  for (x in playlist){
    pl[length(pl) + 1] = as.numeric(df[df$track_uri == x,1] + 1)
  }
  
  pl <- pl[!is.na(pl)]
  pl <- unlist(pl)
  
  #calculate average features for songs in playlist
  pl_avg <- unlist(colMeans(df[pl,5:16]))
  pl_score <- t(data.frame(pl_avg))
  print(pl_avg)
  
  #normalize playlist features
  norm_pl_score <-predict(norm,pl_score)
  
  #classify the playlist into a cluster
  pl_label <- which.min(dist(norm_pl_score,song.kmeans$centers))

  #get all songs that are in the same cluster as playlist
  i <- 0
  song_id <- unlist(full_songs[pl,1])
  song_id <- song_id + 1
  
  reccomendable <- df[-song_id,]
  reccomendable <- df[which(song.kmeans$cluster == pl_label),]
  View(reccomendable)
  
  while (i < 5) {
    rec_dist <- which.min(dist(norm_pl_score,reccomendable[,5:16]))
    print(toString(reccomendable[rec_dist,2:3]))
    reccomendable <- reccomendable[-rec_dist,]
    i <- i + 1
  }
}

recommend(c('spotify:track:0rhI6gvOeCKA502RdJAbfs', 'spotify:track:53H3sGmqiXWO4MwuZAJfyn', 'spotify:track:3IelG5zYpWWCZIH4cqWlPV', 'spotify:track:0f0SbtoKGe41cX5LzlnX6X', 'spotify:track:43mnNatwMMydJ4CxMdQVnJ', 'spotify:track:7F6PtLP6fJPVtA1FWVkl8K', 'spotify:track:51kQvG4aghW17VSZKkfxTp', 'spotify:track:2KwOuFfwQyT9mZqjvchd81', 'spotify:track:08MFgEQeVLF37EyZ7jcwLc', 'spotify:track:7eBpUuPnDTfbeP1P4P93CS', 'spotify:track:0MMIrw3mCssQj4uqJlBzdp', 'spotify:track:6tAPw1Rtt3SGEOm6ohYi8l', 'spotify:track:1dQQ2QlnvXUehsRUrukKmf', 'spotify:track:4dwwfiOzRNlKNUSgfx4Z2k', 'spotify:track:4fVyrplBcb6lVgij1b8pIO', 'spotify:track:28WoBIA4EDVvxiraTv2KZ2', 'spotify:track:6nzCvAtyADh0wwZEVMoujK', 'spotify:track:6dQ4nj3aDaw7WWlI7Rbj34', 'spotify:track:6472TSRvXlqcmg3iSh4GEi', 'spotify:track:3Kw7zkALCVxY4wmlnh2IWC', 'spotify:track:78ByURsaVK39jjwTaerCAk', 'spotify:track:5EAfUqytXYbH0yUZR3QGli', 'spotify:track:5aucVLKiumD89mxVCB4zvS', 'spotify:track:2AOL06oTgW0urXw9MHSWLk', 'spotify:track:0XoTRbjmqOw5Wx7b2aw9Ud', 'spotify:track:3WUyu94psXrYV6HZbXgcFA', 'spotify:track:1E6H8b0dUuCt7vbBgL1Fvm', 'spotify:track:7iRF1psaTyvaVzyTvZhP91', 'spotify:track:6eTTfUgRT21VXq2C2eJ9F3', 'spotify:track:1kDHz6nhxNtdvh209RvyU6', 'spotify:track:1ObmeINgcnxdxGbPByx0zL', 'spotify:track:7aNA0yQbOYu3HKWv9zElJX', 'spotify:track:2anWKmV4Je8175CHvemHV9', 'spotify:track:34JfHOd0fcefm4FSPSrIhF', 'spotify:track:5NcLyVjUgG0yfwHgr5t81w'),full_songs)
recommend(c('spotify:track:0RiRZpuVRbi7oqRdSMwhQY', 'spotify:track:68HocO7fx9z0MgDU0ZPHro', 'spotify:track:3eZqBNHlNA4lUsbAB6UmvD', 'spotify:track:6eDfe957aV4sqGnDhlSikJ', 'spotify:track:2rd4FH1cSaWGc0ZiUaMbX9', 'spotify:track:1PCkbjg2mFmrGAqdP8LB8F', 'spotify:track:39TAKt2hL8sdhVTFCNfm2Y', 'spotify:track:1a1LrfgkTvfHFykIxcv8WU', 'spotify:track:2IAR0DziHCjSu16gR4ihvy', 'spotify:track:4pV3f1Nn5XtyA9x60rFan7', 'spotify:track:2IgbYlOlFpiSFYnsqB39lM', 'spotify:track:6vECYJHxYmm3Ydt3fF01pE', 'spotify:track:2YDj0uV5P7kKN4gGcUMEJT', 'spotify:track:6P2BlKa7uBwKV6KVsEBkmv', 'spotify:track:3XRCW9Ty46AMuKEPFjAZom', 'spotify:track:3GZD6HmiNUhxXYf8Gch723', 'spotify:track:6zvqq50PL7io0rprbkrYc9', 'spotify:track:0nKpHGrnuejevp6openGaF', 'spotify:track:7rgjkzZBhBjObaYsvq8Ej0', 'spotify:track:3wefloF3t1sFZx8YMFhqYB', 'spotify:track:6wcRv0UBvEH9NCIA3IVyQs', 'spotify:track:6z1kLsntE7FuzKZHZWrXYN', 'spotify:track:0QMCxc6dP8ObzoJExJd5oc', 'spotify:track:4ZaRg5Sf4TKr0YcFRLh7QJ', 'spotify:track:4104eILuuQhhcJRkCwGHZE', 'spotify:track:3uA8SjMyDtwtt0jLPMQbVD', 'spotify:track:4EWBhKf1fOFnyMtUzACXEc', 'spotify:track:5unXHHcSQLGO19RuVqZB6d', 'spotify:track:4EpZ4eYuZOwPSSwyqpdHnJ', 'spotify:track:47Bg6IrMed1GPbxRgwH2aC', 'spotify:track:5EYi2rH4LYs6M21ZLOyQTx', 'spotify:track:5OUTFH5acycdnf8OVo21Gv', 'spotify:track:6pXe0HxGT1xufKGhZYqweC', 'spotify:track:22iZ9lazlmjPO9sp0uzUex', 'spotify:track:76tGvMdmFKuiMew33ZNNA9', 'spotify:track:0rKtyWc8bvkriBthvHKY8d', 'spotify:track:2KvbRojbZGMsWr1Nuj5n2f', 'spotify:track:4kEhwajTV4psTYa9FaWwac', 'spotify:track:02tvc9CFnTyHuSRlGeNv9w', 'spotify:track:4RHyIzFRht2z17XeHB9dsF', 'spotify:track:6R5JpLDVRgRI6P2OQCjA4n', 'spotify:track:4ufN2o3Rf6LN5Z0XaqJXIz', 'spotify:track:3PbV3ocgAp3Xn0omGFC0cG', 'spotify:track:2rXVtoLBHuXcNB5DVfIc9p', 'spotify:track:2k9N4caeCIJLOWwWwssrEM', 'spotify:track:1yL848z599Q39USEY7JuDJ', 'spotify:track:7kpe2K0hdSssLj4G4YjxdU', 'spotify:track:21xCXsuI6easPmJ9e25nkl', 'spotify:track:630Ug0XtmhhFvAKo0PNuEI', 'spotify:track:6f7BE3VcZScJx6n3wsf5Z3', 'spotify:track:6sv7ZmasB6qSiF8hWJNpDG', 'spotify:track:12ZYXSzFJUSrFyO1u3Ylqx', 'spotify:track:1dN4Z7wZTQXLEl33RkMO3a', 'spotify:track:5xuYgLzLhNo5nsyLDC2Hse', 'spotify:track:6ZzYETKetIfNUsZUb23jgG', 'spotify:track:1ITJTMrS4cx8zdlI7DdSoo', 'spotify:track:6U0FIYXCQ3TGrk4tFpLrEA', 'spotify:track:6GS3lnAVy5w6AHWEKYzYeS', 'spotify:track:1NCUe90ZCiWTnWn3umrwqw', 'spotify:track:4jbA7VSQHZeWLktuUuHWbz', 'spotify:track:3iGz75NJkqHubYUMRELLyP', 'spotify:track:44uCMdE6RWnkOFYTtbmUUc', 'spotify:track:6Jt96y5MkmIH0b3B8t9IbX', 'spotify:track:50UZJFIsKu5NiFziKZdkmC', 'spotify:track:6sjlDvvIQskeRtipoastyN', 'spotify:track:4Hpi2QkO0Yl8OzirZuyY57', 'spotify:track:2N9udjvetx4OMCA0RLFYEC', 'spotify:track:4Fhlg7UFKHbvBZgsHu6K89', 'spotify:track:2lGQJuNsRG289zdlZmDHR2', 'spotify:track:1Fhb9iJPufNMZSwupsXiRe', 'spotify:track:0EDU6o6WBVoDulghHqLAwT', 'spotify:track:4vHNeBWDQpVCmGbaccrRzi', 'spotify:track:3vWeZSOhhmOai0Go0zKm5j', 'spotify:track:6KhT7CK6FL0BnI3nVPqv4F', 'spotify:track:5jDpJZ4AV0YVV8o9Ez2U9s', 'spotify:track:1EGtfMlDM9qOb0C0vQYz8H', 'spotify:track:14DHQhxhnDnYMi4o4ABfHu', 'spotify:track:4nr88Lwd89TGdccRQEWfsS', 'spotify:track:66dQdXAbtuPdSasezCQVZE', 'spotify:track:7vwhqiIfU8HqXhNgyy8ubR', 'spotify:track:5sZOGj6SfzD7lWpwKhF6oE', 'spotify:track:2P2r0647uy5pKUikGZ7q9B', 'spotify:track:7AYP21Q4qnxw2WxETEvSRb', 'spotify:track:0Yz3F0UGDibDe8uU69zmjn', 'spotify:track:1dA1tlzwcJ3YDYsSul1m06', 'spotify:track:1a9El2p10OoGQ42WoNJpkw', 'spotify:track:4Ompd9kJo9Os4yB0YFzsEC', 'spotify:track:3OYuHSIBy3KrBDXG9Au5r8', 'spotify:track:6dQ4nj3aDaw7WWlI7Rbj34', 'spotify:track:2tHiZQ0McWbtuWaax3dh4P', 'spotify:track:7ArVzlFsFsQXNseVXmdOyk', 'spotify:track:1qJvLRNifrPk9AD14L81zt', 'spotify:track:1hZ2dSvv2O6HQo6p3gDtVO', 'spotify:track:7nyHCuvAMkQKcUOFSg8qSJ', 'spotify:track:5NCOQHI8EUrW1XA13wVzTh', 'spotify:track:2TuWtEwxcwdP63EJ406I82', 'spotify:track:09UAi1gYdutGj4AbDApUki', 'spotify:track:1ko2lVN0vKGUl9zrU0qSlT', 'spotify:track:1VAcUeeWrEqg7j1dwiBstH'),full_songs)
recommend(c('spotify:track:7IXi8qEDc9oASO5LTx19nT', 'spotify:track:7jqlogJxaMcbBEazMDpgnL', 'spotify:track:5H66Hq2HgWs3US0qJvztia', 'spotify:track:1f8y9dfpCSBW7BJRfOjop7', 'spotify:track:0FxShF2ad0XegLlZ4UPImP', 'spotify:track:1xEGxyC38Ms7uq5C5jlgXD', 'spotify:track:10w5yROKTtdjmS00FmaGn4', 'spotify:track:1UxYNCzvOgMMMvzQe66NIY', 'spotify:track:18Ptr7mwqViAKSrUZ0WyHm', 'spotify:track:326Di9nEnC8tzxxtBsjOUv', 'spotify:track:5pesNiBKAx8JNwK2mQ2HEc', 'spotify:track:7qA1ly9O3kKuUIJD3Ia4u3', 'spotify:track:611i6vK3EiJqemBRO0ZPW6', 'spotify:track:5ptViFrfoKdGPTdd9oQCwr', 'spotify:track:1Acgi7wxVD9OF3iQnFe8Q4', 'spotify:track:6NljjfZDdfVh8x31Dwvj7J', 'spotify:track:6lRGSp6vkzCvrHSwqO9Tx6', 'spotify:track:0mSYnR1ubvlot6XQlfZ6vI', 'spotify:track:6KvLQJm1B7lP3BS8AuMv7B', 'spotify:track:7rxnGhTlqU3FRUzd8F4R3d', 'spotify:track:31PzY79H10HCgJs533Xq6B', 'spotify:track:3Q5Zs0EH4d7b00xMw3vwSJ', 'spotify:track:1vXsCZjGd1LW6EVwcPhMc7', 'spotify:track:1Nm10NmlVLhzldj1ALxlWV', 'spotify:track:4XLd7gMhD6Jo2tuHxNaIlq', 'spotify:track:1nhMnpxfNIIkViyCDNQvNH', 'spotify:track:1VwOad23MsNZkmDyIyxEXe', 'spotify:track:0vZtHRT1Ez9xmmxGRCNF8h', 'spotify:track:5BORybllcNNjapY2shRHy1', 'spotify:track:1HYzRuWjmS9LXCkdVHi25K', 'spotify:track:7lX7xZA7IJ10ga8mqZnf0j', 'spotify:track:7rLvsAO1yb7ElxPhkz60qh', 'spotify:track:1gyhtYG9OWOZvhZzDVF6lq', 'spotify:track:1c4zpHXMMlGNToAuFJjsyg', 'spotify:track:3pnvvqhkOYq0kTgLaT4BCA', 'spotify:track:3GTRCT1awGpTwi3ONuT8D3', 'spotify:track:6zDfmmeyBbUBti3KhVhiyf', 'spotify:track:214fNrcCdFO4BDDZhejst0', 'spotify:track:23ZOetlqB0jACaFRStNmyO', 'spotify:track:629NRMzA7LTXSuzwG7R1Ag', 'spotify:track:54Nx0KMNIIA0tAPewmd69X', 'spotify:track:0WNGsQ1oAuHzNTk8jivBKW', 'spotify:track:5lEvC4905fBtL9GjJKionT', 'spotify:track:3HqHJaYsBVVDWAe9ha5fGh', 'spotify:track:59Pyly0sGOkIdf1n1ctC7j', 'spotify:track:4kVNcd29xNwseUPJZEck5Z', 'spotify:track:4cKIpKvM7mmA4ARJC1Jpq5', 'spotify:track:7mW8ar9hy7GSeH4lohyOKs', 'spotify:track:2TXb9wFuNJiLgiMBBvtyVO', 'spotify:track:42HB7NqTtooRK7Lrwvwrq2', 'spotify:track:4WEDwbFzxYsd28HZtuG7Yd', 'spotify:track:7jb6GXWNyeGUKFaUCpmNX1', 'spotify:track:6uHoPVdPEqXlH1Kd3KqTIJ', 'spotify:track:4r3k2iBOPJZHqnmkOFMJEv', 'spotify:track:54s6sdPuMjVtdB5d5KFzTC', 'spotify:track:1b1I5xYkIu5ZGujUzK79Kg', 'spotify:track:41okbOiOkWPI0OIVm6XMgE', 'spotify:track:3hm4axDodp2jFEDohQfXxV', 'spotify:track:5XXJnC5TvcL2QsAZ3Nxgku', 'spotify:track:2Ghu1DdMwxS3VAyB7i38Wo', 'spotify:track:5WEUUZ4UdbH2exVjtZCGUh', 'spotify:track:4a1FFgdBIbAvyn4d40YAPZ', 'spotify:track:3NxuezMdSLgt4OwHzBoUhL', 'spotify:track:3SfbB0Y3saMIQnNctxMVhj', 'spotify:track:31YajbWhSNIvtEyU6lwoSK', 'spotify:track:5ZkITfPpcNPnyYGTibkO6m', 'spotify:track:6mzF8HvHdVrzJNd8M1uFCS', 'spotify:track:1afMrs1HEsNDm11GRrLeoW', 'spotify:track:3qbsRctNTLFSgCvz6ZKYjW', 'spotify:track:1HWzxWJhNcBaGq53aF0ZTh', 'spotify:track:5Z6g2vnqBgd23HNRLDWoBY', 'spotify:track:6eqtsVnkxd5VUtKXzygv8k', 'spotify:track:5JkLv6htBEmt93ODYXubst', 'spotify:track:39Xa43tlDYeCoDvVyrjs3g', 'spotify:track:3UmpTGo1PtEYGpr2aqXHQ6', 'spotify:track:3MNJzWgUmu0X9tQxRuww8N', 'spotify:track:6nUpdTLUSel3Zp6lhHZRbX', 'spotify:track:7x2PMStZn3xJqeLXHODckx', 'spotify:track:4TWHREp4wv0TmewqR6rgRd', 'spotify:track:1RMUSljuiZKUNaf6xskK9n', 'spotify:track:3M0qHiwpoXUMffAS1CBB6M', 'spotify:track:1A7pDII9Zy07oCDW3xldgy', 'spotify:track:0iBh2ZfirDzFDkqrvkvoiW', 'spotify:track:6m6PTfVXdPZT6scb1TaONP', 'spotify:track:0ZH8lETOZubOiqol2YbFyN', 'spotify:track:3witRpHWHqArlnt6GTlH2Y', 'spotify:track:2rk5DdEFF6wJ13Wrwm2sy6', 'spotify:track:3bSrpGWHrMlwZ0TdBMXv1p', 'spotify:track:5oXdWFCLkBX5XmGbTrlK40', 'spotify:track:3bHKTyjnhkhCOpTDYTMZmN', 'spotify:track:33txTXgQioQ5eaXc7yWd67', 'spotify:track:03fnJaS2QkeMDNrLq1d2WX', 'spotify:track:1NDpztr7wYJKDX0LSKnyzV', 'spotify:track:0GVKkD74xcixRYYG3zv1NL', 'spotify:track:5CDq3Ekm0E0c23S0mbEELV', 'spotify:track:1gDAicAbgxCYQQ9NDJ5eST', 'spotify:track:30NvzcboJe3GQRIDkJM0k3', 'spotify:track:4hMYovC1APX8ojuANCoeuF', 'spotify:track:4mtPGvoOvfqskC9M3tNZrV', 'spotify:track:3diZHzXj5LNxztVwBjnebn'),full_songs)