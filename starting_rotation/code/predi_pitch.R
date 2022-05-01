foreach(id = 1:30, .combine = rbind) %do% {
  
  # load pitcher data
  fname_pitcher <- paste0("pitcher", id, ".RDs")
  load(file = file.path(path, 'output', 'pitchers', fname_pitcher))
  
  # train test split a different way
  pitcher_split <- partition(data = pitcher, p = .8, cat_col = "pitch_type")
  pitcher_train <- pitcher_split[[1]]
  pitcher_test <- pitcher_split[[2]]
  
  # re-factor
  pitcher_train$pitch_type <- factor(pitcher_train$pitch_type)
  pitcher_test$pitch_type <- factor(pitcher_test$pitch_type)
  
  fname_testset <- paste0("pitcher_test", id, ".Rds")
  save(file = file.path(path, 'output', 'pitchers', fname_testset), list = "pitcher_test")
  
  # situation only
  trashcan_1 <- randomForest(pitch_type ~ 
                               inning + 
                               b_count + 
                               s_count +
                               pitch_num +
                               stand +
                               on_1b + 
                               on_2b + 
                               on_3b +
                               outs +
                               p_score +
                               b_score,
                             data = pitcher_train, importance = TRUE, 
                             na.action = na.exclude)
  
  # situation and previous 
  trashcan_2 <- randomForest(pitch_type ~ 
                               inning + 
                               b_count + 
                               s_count +
                               pitch_num +
                               stand +
                               on_1b + 
                               on_2b + 
                               on_3b +
                               outs +
                               p_score +
                               b_score +
                               prev_pitch +
                               prev_pitch2 +
                               prev_event,
                           data = pitcher_train, importance = TRUE, 
                           na.action = na.exclude)
  
  # all features
  trashcan_3 <- randomForest(pitch_type ~ . - zone - ab_id - g_id - first_pitch,
                             data = pitcher_train, importance = TRUE, 
                             na.action = na.exclude)
  
  
  fname_model <- paste0("trashcans", id, ".RData")
  save(file = file.path(path, 'output', 'models', fname_model), list = c("trashcan_1", "trashcan_2", "trashcan_3"))
}

