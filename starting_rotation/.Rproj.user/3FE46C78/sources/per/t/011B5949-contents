

pitches <- read.csv(file.path(path, 'data', 'raw', 'pitches.csv'))

atbats <- read.csv(file.path(path, 'data', 'raw', 'atbats.csv'))

pitches <- merge(pitches, atbats)

pitches <- pitches %>% 
  mutate(first_pitch = ifelse(pitch_num == 1, 1, 0), 
         prev_pitch = ifelse(first_pitch == 0, lag(pitch_type), "none")) %>% 
  dummy_cols(select_columns = "prev_pitch")
