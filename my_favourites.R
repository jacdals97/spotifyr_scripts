#set a working directory if you want


name <- "" #please put your name here so that I know who is who

install.packages("dplyr", dependencies = T)
install.packages("glue", dependencies = T)
install.packages("fansi", dependencies = T)
install.packages("devtools", dependencies = T)

library(devtools)
install_github('charlie86/spotifyr', force = T)
3
#if 3 doesn't work try 1
library(pacman)

p_load(dplyr, googledrive, tibble, spotifyr)


#Insert keys from spotify api
Sys.setenv(SPOTIFY_CLIENT_ID = "")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "")



file_name <- paste0("mft_", name, ".csv")


my_favorite_tracks <- tibble(get_my_top_artists_or_tracks(type = "tracks", limit = 50, time_range = "long_term",
                                                   authorization = get_spotify_authorization_code(scope = "user-top-read"))[,c(7:10,14)],
                             period = "all_time") %>% 
  bind_cols(get_track_audio_features(.$id)) 

my_favorite_tracks <- bind_rows(my_favorite_tracks,
                                tibble(get_my_top_artists_or_tracks(type = "tracks", limit = 50, time_range = "medium_term",
                                                authorization = get_spotify_authorization_code(scope = "user-top-read"))[,7:10],
                   period = "medium_term") %>% 
  bind_cols(get_track_audio_features(.$id)))

my_favorite_tracks <- bind_rows(my_favorite_tracks,
  tibble(get_my_top_artists_or_tracks(type = "tracks", limit = 50,  time_range = "short_term",
                                                authorization = get_spotify_authorization_code(scope = "user-top-read"))[,7:10],
                   period = "short_term") %>%
    bind_cols(get_track_audio_features(.$id)))
  
  

write.csv(my_favorite_tracks, file_name)


