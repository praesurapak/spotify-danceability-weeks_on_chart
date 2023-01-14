install.packages("dplyr")
library(dplyr)

## Data
spotifydf <- read.csv("Group18_Final_Spotify.csv", sep=",", header=T)
spotifydf <- select(spotifydf, -X, -X.1)

## Linear Model
options(scipen = 999)
model1 <- lm(LOG.weeks_on_chart ~ LOG.danceability, 
             data = spotifydf)
summary(model1)

model2 <- lm(LOG.weeks_on_chart ~ LOG.danceability + tiktok + streams 
             + loudness + LOG.energy, 
             data = spotifydf)
summary(model2)

cor(spotifydf[,c("LOG.weeks_on_chart", "LOG.danceability", "tiktok", "streams",
                 "loudness", "LOG.energy")]) 


model3 <- lm(LOG.weeks_on_chart ~ LOG.danceability + tiktok + streams + loudness, 
             data = spotifydf)
summary(model3)


