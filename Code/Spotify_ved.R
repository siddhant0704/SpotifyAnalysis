#install.packages('tidyverse')
#install.packages('dplyr')
library(ggplot2)
library('dplyr')
library('tidyverse')
data = read.csv('Spotify_songs - spotify_songs.csv')

set.seed(5702)
newdata <- data
newdata[1:25, "playlist_name"] <- NA
newdata[100:115, "energy"] <- NA
newdata[1000:1120, 5:8] <- NA
newdata[1:52, "danceability"] <- NA
newdata[2100:2215, "tempo"] <- NA

colsna = colSums(is.na(newdata)) %>%
  sort(decreasing = TRUE)

cols = colnames(newdata)
missing_na = c()

for (i in colsna){
  
  missing_na = append(missing_na, i)
  
}

dfna = data.frame(cols, missing_na)

dftf = dfna %>% mutate(missing = ifelse(missing_na == 0, NA, missing_na))



ggplot(dfna, aes(x=reorder(cols, -missing_na), y=missing_na)) + 
  geom_bar(stat = "identity")


ggplot(dfna, aes(x=reorder(cols, +missing_na), y=missing_na)) + 
  geom_point(color = "blue") + coord_flip()

ggplot(dftf, aes(x = cols, y = missing_na, fill = missing)) +
  geom_tile(color = "white") + 
  scale_fill_gradient(low = "yellow", high = "red", na.value = "black") + theme_bw()



a = newdata%>%group_by(track_artist)%>%summarise(danceability == NA)

colsna = colSums(is.na(newdata)) %>%
  sort(decreasing = TRUE)

popartist = newdata %>%
  group_by(track_artist) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n)) %>% filter(n > 50)
ß
modified_data = newdata%>%group_by(track_artist) %>% mutate(n = n()) %>% filter(n > 50)

m1 <- modified_data %>% 
  rownames_to_column("id") %>% 
  gather(key, value, -id) %>% 
  mutate(missing = ifelse(is.na(value), "yes", "no"))

ggplot(m1, aes(x = key, y = fct_rev(id), fill = missing)) +
  geom_tile(color = "white") + 
  ggtitle("mtcars with NAs added") +
  scale_fill_viridis_d() + # discrete scale
  theme_bw()


