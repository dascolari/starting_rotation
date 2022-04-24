foreach(id = 1:30, .combine = rbind) %do% {
  
  # load pitcher data
  fname_pitcher <- paste0("pitcher", id, ".RDs")
  load(file = file.path(path, 'output', 'pitchers', fname_pitcher))
  
  # train test split a different way
  pitcher_split <- partition(data = pitcher, p = .8, cat_col = "pitch_type")
  pitcher_train <- pitcher_split[[1]]
  pitcher_test <- pitcher_split[[2]]
  
  # re-factor like a little b****
  pitcher_train$pitch_type <- factor(pitcher_train$pitch_type)
  pitcher_test$pitch_type <- factor(pitcher_test$pitch_type)
  
  fname_testset <- paste0("pitcher_test", id, ".Rds")
  save(file = file.path(path, 'output', 'pitchers', fname_testset), list = "pitcher_test")
  
  # holy shit please fucking work
  trashcan <- randomForest(pitch_type ~ . - zone - ab_id - g_id,
                           data = pitcher_train, importance = TRUE, 
                           na.action = na.exclude)
  fname_model <- paste0("trashcan", id, ".RDs")
  save(file = file.path(path, 'output', 'models', fname_model), list = "trashcan")
}
