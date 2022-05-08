# Enter the game situation 

id = 12
stance = "R"
strikes = 1
balls = 1
out = 1

# Call the pitcher's dataset 

load(file.path(path, "output", "pitches_import.RData"))

fname_model <- paste0("trashcans", id, ".RData")
load(file = file.path(path, 'output', 'models', fname_model))

fname_pitcher <- paste0("pitcher", id, ".Rds")
load(file = file.path(path, 'output', 'pitchers', fname_pitcher))

fname_factor <- paste0("pitcher_factor", id, ".RDs")
load(file = file.path(path, 'output', 'pitchers', fname_factor))

pitcher_first <- names$first_name[id]
pitcher_last <- names$last_name[id]

# affix the model prediction results to the pitches dataset

pitcher$pitch_type <- factor(pitcher$pitch_type, factor_types)
pitcher_predi <- pitcher %>% 
  mutate(pitchhat_trashcan = predict(trashcan_3, pitcher))
pitcher_predi$pitchhat_trashcan = factor(pitcher_predi$pitchhat_trashcan, factor_types)

# filter by game situation

  pitcher_result <- pitcher_predi %>% 
    filter(
      stand == stance &          
        s_count == strikes &
           b_count == balls &
           outs == out
           )

# summarize the predictions and the actual results 
  
  pitcher_guess <- pitcher_result %>% 
    group_by(pitchhat_trashcan) %>% 
    summarize(prediction = n()) %>% 
    mutate(percent = prediction / sum(prediction)) %>% 
    rename(pitch_type = pitchhat_trashcan)
  
  pitcher_real <-  pitcher_result %>% 
    group_by(pitch_type) %>% 
    summarize(actual = n()) %>% 
    mutate(actual_percent = actual / sum(actual))

  situation <- merge(pitcher_guess, pitcher_real)

  situation <-situation %>% 
    arrange(desc(percent))
  
situation  
