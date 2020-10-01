library(pacman)

p_load(tidyverse, spotifyr, RColorBrewer, wordcloud)

files_list <- list.files(pattern = "mft_")

loader <- function(filename){
  name <- read.csv(filename)
  name$name <- str_extract(filename, "[:alpha:]{4,10}")
  return(name)
}


df <- map_df(files_list, loader)

df <- df[,2:6]

#this part is very weird but the api only allows 100 retrievals per request so I made some dumb not generalizable loops
#please change them

for (i in 0:9){
my_favorite_tracks[(i*98+1):((i+1)*98),] <- bind_cols(get_track_audio_features(df$id[(i*98+1):((i+1)*98)]),
                                                        df[(i*98+1):((i+1)*98),])
}
my_favorite_tracks <- my_favorite_tracks[,1:24]


for (i in 1:980){
  my_favorite_tracks[i,25:26] <- bind_cols(get_track(df$id[i])[c(6,12)], my_favorite_tracks[i,])
}


art_list <- c()
for (i in 1:980){
  art_list[i] <- c(get_track(df$id[i])[[2]][2][1])}


for (i in 1:980){
  my_favorite_tracks[i,27:28] <- bind_cols(get_artist(art_list[[i]][1])[7:8], my_favorite_tracks[i,])
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

my_favorite_tracks$loudness_norm <- normalize(my_favorite_tracks$loudness)
my_favorite_tracks$tempo_norm <- normalize(my_favorite_tracks$tempo)

colnames(my_favorite_tracks)[25:28] <- c("explicit", "title", "artist", "artist_pop")



top_songs <- my_favorite_tracks %>% group_by(title) %>% summarise(freq = n())
topsongs_all <- my_favorite_tracks %>% filter(period == "all_time") %>%  group_by(title) %>% summarise(freq = n())

wordcloud::wordcloud(top_songs$title, top_songs$freq, scale=c(1.5,0.2), min.freq = 2, max.words = 50, 
                     random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))


top_artists <- my_favorite_tracks %>% group_by(artist) %>% summarise(freq = n())


wordcloud::wordcloud(top_artists$artist, top_artists$freq, scale=c(1.5,0.2), min.freq = 2, max.words = 50, 
                     random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))



wordcloud2(data=top_songs, size=1.6, color='random-dark')


words <- str_remove_all(my_favorite_tracks$title, "[:punct:]")
words <- tolower(words)
words <- str_split(words, " ")

words2 <- words[[1]]
for (i in 2:980){
  words2 <- c(words2, words[[i]])
}

words2 <- as.data.frame(words2)
top_words <- words2 %>% group_by(words2) %>% summarise(freq = n())

wordcloud::wordcloud(top_words$words2, top_words$freq, scale=c(6,.5), min.freq = 5, max.words = 200, 
                     random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#valence
my_favorite_tracks %>% group_by(name) %>% summarise(mean = mean(as.numeric(valence), na.rm = T))

#danceability
my_favorite_tracks %>% group_by(name) %>% summarise(mean = mean(as.numeric(danceability), na.rm = T))

#explicit
mft <- my_favorite_tracks %>% group_by(name, explicit) %>% summarise(count = n())
mft <- mft[mft$explicit == T,]


#
my_favorite_tracks %>% group_by(name) %>% summarise(mean = mean(as.numeric(popularity), na.rm = T))

genre <- c()
for (i in 1:980){
  genre[i] <- get_artist(art_list[[i]][1])[3]
}


genres <- genre[[1]]
for (i in 2:980){
  genres <- c(genres, genre[[i]])
}
genres <- as.data.frame(unlist(genres))
top_genres <-  genres %>% group_by(unlist(genres)) %>% summarise(freq = n())

wordcloud::wordcloud(top_genres$`unlist(genres)`, top_genres$freq, scale=c(3,.4), min.freq = 5, max.words = 200, 
                     random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))



my_favorite_tracks %>% group_by(name) %>% summarise(mean = mean(as.numeric(duration_ms), na.rm = T))

dogP <- get_track_audio_analysis("6OXT1Ga5d7YyQaEXK7JCpN")



all_times <- my_favorite_tracks %>% filter(period == "all_time")
all_time_artist <- all_times %>% group_by(artist) %>% summarise(freq = n())
