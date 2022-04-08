# spring training tryouts - harrison
pitches <- read.csv(file.path(path, 'data', 'raw', 'pitches', 'pitches.csv'))
atbats <- read.csv(file.path(path, 'data', 'raw', 'archive', 'atbats.csv'))
pitches <- merge(pitches, atbats) 
