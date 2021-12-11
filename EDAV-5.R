
library(tidyverse)
library(openintro)
library(agridat)
library(boot)
library(ggridges)
library(plotly)
# install.packages("patchwork")
library(patchwork)

data <- read.csv('C:/Users/Sid/Downloads/Spotify_final_data.csv')
data = subset(data, select = -c(X))

#How does the danceability feature relate to songs produced in different eras? Do ppl
#dance to music produced in the current era or the preceding era?

colsna = colSums(is.na(data)) %>%
  sort(decreasing = TRUE)

cols = colnames(data)
#Remove NA entries
data = data %>% filter(!is.na(playlist_name) & !is.na(track_album_name) & !is.na(track_album_id) & !is.na(track_album_release_date) & !is.na(tempo) & !is.na(danceability) & !is.na(energy) & !is.na(track_name) & !is.na(track_artist))
#Add a year column in the dataset
data$year = as.numeric(substring(data$track_album_release_date,1,4))
#removing unnecessary colums from the data
data = data%>%dplyr::select(-track_id,-track_album_id,-playlist_id)

------------------------------------------------------------------------------------------------------
# Ridge density plot for Dancibility/ Energy/ Loudness / Speechiness for each genre
  
library(ggridges)
density_plot_d = data %>% ggplot() + 
  geom_density_ridges(aes(danceability , playlist_genre,fill = playlist_genre), alpha = .3)


density_plot_e = data %>% ggplot() + 
  geom_density_ridges(aes(energy, playlist_genre, fill = playlist_genre),alpha = .3)


density_plot_l = data %>% ggplot() + 
  geom_density_ridges(aes(loudness, playlist_genre, fill = playlist_genre), alpha = .3)

density_plot_s = data %>% ggplot() + 
  geom_density_ridges(aes(speechiness, playlist_genre, fill = playlist_genre), alpha = .3)


g <-  ((density_plot_d + density_plot_e) / (density_plot_l + density_plot_s)) 

g

--------------------------------------------------------------------------------------------------------
# treemap depicts top 15 track artists with in each of the 6 playlist genre. The size of the boxes in treemap corresponds to the count tracks for the artists.
library(treemap) 
library(viridis)
top_genre <- data %>% select(playlist_genre, track_artist, track_popularity) %>% group_by(playlist_genre,track_artist) %>% summarise(n = n()) %>% top_n(15, n)

tm <- treemap(top_genre, index = c("playlist_genre", "track_artist"), vSize = "n", vColor = 'playlist_genre', palette =  viridis(6),title="Top 15 Track Artists within each Playlist Genre")

----------------------------------------------------------------------------------------------------------

# Parallel Plot
data_2019 = data[(data$year == "2019" | data$year == "2018"),]


data_new <- aggregate(cbind(`liveness`,`valence`,`acousticness`,`speechiness`, `energy`) ~ playlist_subgenre, data = data_2019, FUN = sum, na.rm = TRUE)

data_new$mode = data_2019$mode[match(data_new$playlist_subgenre, data_2019$playlist_subgenre)]

parcoords::parcoords(data_new,
                     color = list( colorScale = "scaleOrdinal",
                                   colorBy = "mode",
                                   colorScheme = "schemeCategory10"),
                     withD3 = TRUE,
                     brushMode = "1D-axes-multi", 
                     alphaOnBrushed = 5,
                     reorderable = TRUE, 
                     queue = TRUE,
                     alpha = 0.5) 

-------------------------------------------------------------------------------------------------------------
data1 = data %>% select(energy, danceability, duration_ms, tempo, loudness, popularity) %>% filter(popularity >= 0.5)
     
g1 = ggplot(data1, aes(danceability, energy )) +
  geom_hex(bins = 10) + #binwidth = c(5, 5)
  scale_fill_gradient(low = "#cccccc", high = "#09005F") + # color
  labs(y = "energy", x = "danceability") +
  ggtitle(" Hexagonal heatmap of danceability and energy") + theme_minimal()


g2 = ggplot(data1, aes(duration_ms, energy )) +
  geom_hex(bins = 10) + #binwidth = c(5, 5)
  scale_fill_gradient(low = "#cccccc", high = "#09005F") + # color
  labs(y = "energy", x = "duration_ms") +
  ggtitle("Hexagonal heatmap of duration in ms and energy") + theme_minimal()


g3 = ggplot(data1, aes(loudness, energy )) +
  geom_hex(bins = 10) + #binwidth = c(5, 5)
  scale_fill_gradient(low = "#cccccc", high = "#09005F") + # color
  labs(y = "energy", x = "loudness") +
  ggtitle("Hexagonal heatmap of loudness and energy") + theme_minimal()


g4 = ggplot(data1, aes(tempo, energy )) +
  geom_hex(bins = 10) + #binwidth = c(5, 5)
  scale_fill_gradient(low = "#cccccc", high = "#09005F") + # color
  labs(y = "energy", x = "tempo") +
  ggtitle("Hexagonal heatmap of tempo and energy") + theme_minimal()

#data1 = data %>% mutate(eng = energy/sum(energy)) %>% filter(eng>0.5) %>% select(eng, playlist_genre)
g<- (g1 + g2)/ (g3 + g4)
g

--------------------------------------------------------------------------------------------------------

  minutesMostListened = data %>% 
  group_by(track_artist) %>% 
  summarize(minutesListened = sum(duration_ms)/60000) %>% 
  filter(minutesListened >= 180) %>%
  ggplot(aes(x = track_artist, y = minutesListened)) + 
  geom_col(aes(fill = minutesListened)) +
  scale_fill_gradient(low = "yellow", high = "red") + 
  labs(x= "Artist", y= "Minutes of music playback") + 
  ggtitle("What were the most listened artists on my Spotify?", "> 3 hours listened") +
  theme(axis.text.x = element_text(angle = 90))
minutesMostListened