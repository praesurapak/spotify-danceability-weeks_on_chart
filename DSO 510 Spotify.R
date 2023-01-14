getwd()
setwd("~/Desktop/USC/DSO 510/Project")
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library("tidyr")
options(scipen = 999)

spotifydf = read.csv(file = "spotify_top_charts_22.csv", 
                     header = T, as.is = T, fileEncoding="UTF-8-BOM")

str(spotifydf)
summary(spotifydf) # There is no N/A values

spotifydf  = select(spotifydf, -uri)
# remove the featuring artists, left only the main artists
spotifydf$artist_names <- gsub(",.*", "", spotifydf$artist_names)


tiktokdf = read.csv(file = "TikTok_songs_2022.csv", 
                    header = T, as.is = T, fileEncoding="UTF-8-BOM")
summary(tiktokdf) # There is no N/A values
tiktokdf  = select(tiktokdf, track_name, artist_name)

artistdf = read.csv(file = "spotify_artist_data.csv", 
                    header = T, as.is = T, fileEncoding="UTF-8-BOM")

artistdf  = select(artistdf, -X, -Feats, -Tracks, -One.Billion, -Last.Updated)
artistdf = artistdf %>% 
  mutate_all(na_if,"") %>% 
  drop_na()

artistdf$Lead.Streams <- as.numeric(gsub(",","",artistdf$Lead.Streams))
artistdf$X100.Million <- as.numeric(artistdf$X100.Million)

colnames(artistdf) <- c('artist_names',
                        'streams',
                        '100M')

df_merge <- merge(spotifydf,artistdf,by="artist_names")

df_merge <- df_merge[, c(1, 17, 18, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
                         12, 13, 14, 15, 16)]
# remove duplicate
df_merge <- df_merge[-c(101), ]

df = df_merge %>%
  mutate(tiktok = c(0, 1)[1 + (rowSums(
    outer(
      strsplit(track_name, "\\s+"),
      strsplit(tiktokdf$track_name, "\\s+"),
      Vectorize(function(x, y) all(x %in% y) | all(y %in% x))
    )
  ) > 0)])

sum(df$tiktok) # 74 tiktok songs

finaldf <- df[, c(1, 2, 3, 4, 19, 5, 6, 7, 8, 9, 10,
                  11, 12, 13, 14, 15, 16, 17, 18)]

write.csv(finaldf,"/Users/prae/Desktop/USC/DSO 510/Final_Spotify.csv", 
          row.names = TRUE)

cor(finaldf$weeks_on_chart, finaldf$danceability)
cor(finaldf$weeks_on_chart, finaldf$streams)
cor(finaldf$danceability, finaldf$peak_rank)

