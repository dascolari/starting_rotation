pitches <- read.csv(file.path(path, 'data', 'raw', 'pitches', 'pitches.csv'))
atbats <- read.csv(file.path(path, 'data', 'raw', 'archive', 'atbats.csv'))
names <- read_xlsx(file.path(path, 'data', 'raw', 'archive', 'first_lineup.xlsx'))
pitches <- merge(pitches, atbats, by = "ab_id") 
pitches <- merge(pitches, names, by.x = "pitcher_id", by.y = "id")

save(file = file.path(path, "output", "pitches_import.RData"), list = c("pitches", "names"))
