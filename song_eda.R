library(readr)
songs <- read_csv("songs.csv")
View(songs)
attach(songs)

plot(loudness,energy)

plot(valence,danceability)
abline(reg = lm(danceability~valence), col="red",lwd=5)

plot(acousticness,loudness)
abline(reg = lm(loudness~acousticness), col="red",lwd=5)

plot(danceability,popularity)

minor <- songs[songs$mode == 0.0,]
major <- songs[songs$mode == 1.0,]
mean(minor$valence)
mean(major$valence)

hist(major$valence)
hist(minor$valence)
hist(minor$tempo)
hist(major$tempo)
hist(major$speechiness)
hist(minor$speechiness)
hist(minor$danceability)
hist(major$danceability)

plot(energy,tempo)
plot(tempo,energy)

popular_songs <- songs[popularity > median(popularity),]
unpopular_songs <- songs[popularity < median(popularity),]
