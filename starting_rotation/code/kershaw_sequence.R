load(file.path(path, "output", "pitchers","pitcher12.RDs"))

# illustrate how kershaw changes his strategy depending on the situation
all <- pitcher %>% 
  group_by(pitch_type) %>% 
  summarise(count = length(pitch_type), .groups = 'drop') %>% 
  mutate(All = round(count/sum(count), 3)) %>% 
  arrange(desc(count)) %>% 
  dplyr::select(pitch_type, All)

first_pitch <- pitcher %>% 
  filter(pitch_num == 1) %>%
  group_by(pitch_type) %>% 
  summarise(count = length(pitch_type), .groups = 'drop') %>% 
  mutate(first_pitch = round(count/sum(count), 3)) %>% 
  dplyr::select(pitch_type, first_pitch)

after_ff <- pitcher %>% 
  filter(prev_pitch == "FF") %>%
  group_by(pitch_type) %>% 
  summarise(count = length(pitch_type), .groups = 'drop') %>% 
  mutate(after_ff = round(count/sum(count), 3)) %>% 
  dplyr::select(pitch_type, after_ff)

two_strikes <- pitcher %>% 
  filter(s_count == 2) %>%
  group_by(pitch_type) %>% 
  summarise(count = length(pitch_type), .groups = 'drop') %>% 
  mutate(two_strikes = round(count/sum(count), 3)) %>% 
  dplyr::select(pitch_type, two_strikes)

three_balls <- pitcher %>% 
  filter(b_count == 2) %>%
  group_by(pitch_type) %>% 
  summarise(count = length(pitch_type), .groups = 'drop') %>% 
  mutate(three_balls = round(count/sum(count), 3)) %>% 
  dplyr::select(pitch_type, three_balls)

heart <- pitcher %>% 
  filter(slot == 3 | slot == 4) %>%
  group_by(pitch_type) %>% 
  summarise(count = length(pitch_type), .groups = 'drop') %>% 
  mutate(heart = round(count/sum(count), 3)) %>% 
  dplyr::select(pitch_type, heart)

bottom <- pitcher %>% 
  filter(slot > 7) %>%
  group_by(pitch_type) %>% 
  summarise(count = length(pitch_type), .groups = 'drop') %>% 
  mutate(bottom = round(count/sum(count), 3)) %>% 
  dplyr::select(pitch_type, bottom)

lefties <- pitcher %>% 
  filter(stand == "L") %>%
  group_by(pitch_type) %>% 
  summarise(count = length(pitch_type), .groups = 'drop') %>% 
  mutate(lefties = round(count/sum(count), 3)) %>% 
  dplyr::select(pitch_type, lefties)
  
df_list <- list(all, first_pitch, after_ff, 
                two_strikes, three_balls, heart, 
                bottom, lefties)

pitch_sequence <- df_list %>% reduce(full_join, by = "pitch_type") %>% 
  kable(caption = "Clayton Kershaw: Condistional Pitch Arsenal")

# illustrate how interactions with high specificity can improve our predictions 
just_walked_4 <- pitcher %>% 
  filter(prev_event == "Walk" & pitch_num == 1 & slot == 4) %>%
  group_by(pitch_type) %>% 
  summarise(count = length(pitch_type), .groups = 'drop') %>% 
  mutate(just_walked_4 = round(count/sum(count), 3)) %>% 
  dplyr::select(pitch_type, just_walked_4)

just_walked_bottom <- pitcher %>% 
  filter(prev_event == "Walk" & pitch_num == 1 & slot > 7) %>%
  group_by(pitch_type) %>% 
  summarise(count = length(pitch_type), .groups = 'drop') %>% 
  mutate(just_walked_bottom = round(count/sum(count), 3)) %>% 
  dplyr::select(pitch_type, just_walked_bottom)

just_walked <- merge(just_walked_4, just_walked_bottom, 
                     by = "pitch_type", all.x = T) %>% 
  mutate(just_walked_bottom = 
           ifelse(is.na(just_walked_bottom) == T, "-", just_walked_bottom)) %>% 
  arrange(desc(just_walked_4))

colnames(just_walked) <- c("pitch_type", "4 Hitter", "8-9 Hitters")

just_walked <- kable(just_walked, 
                     caption = "Clayton Kershaw: First Pitch After a Walk")

save(file = file.path(path, 'output', 'tables', 'kershaw_sequence.RData'), 
     list = c("pitch_sequence", "just_walked"))