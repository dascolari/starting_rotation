pitches <- read.csv(file.path(path, 'data', 'raw', 'pitches', 'pitches.csv'))

atbats <- read.csv(file.path(path, 'data', 'raw', 'archive', 'atbats.csv'))

batting_order <- atbats %>% 
  group_by(g_id, ab_id, top) %>% 
  summarise(slot = 1, .groups = 'drop') %>% 
  arrange(top, g_id, ab_id) %>% 
  group_by(g_id, top) %>% 
  mutate(slot = case_when(row_number()%%9 == 0 ~ 9,
                          (row_number()+1)%%9 == 0 ~ 8,
                          (row_number()+2)%%9 == 0 ~ 7,
                          (row_number()+3)%%9 == 0 ~ 6,
                          (row_number()+4)%%9 == 0 ~ 5,
                          (row_number()+5)%%9 == 0 ~ 4,
                          (row_number()+6)%%9 == 0 ~ 3,
                          (row_number()+7)%%9 == 0 ~ 2,
                          (row_number()+8)%%9 == 0 ~ 1)) %>% 
  ungroup() %>% 
  dplyr::select(ab_id, slot)

atbats <- merge(atbats, batting_order)

names <- read_xlsx(file.path(path, 'data', 'raw', 'archive', 'first_lineup.xlsx'))

pitches <- merge(pitches, atbats, by = "ab_id") 
pitches <- merge(pitches, names, by.x = "pitcher_id", by.y = "id")

save(file = file.path(path, "data", "atbats.RDs"), list = "atbats")
save(file = file.path(path, "data", "pitches.RDs"), list = "pitches")
save(file = file.path(path, "data", "names.RDs"), list = "names")
