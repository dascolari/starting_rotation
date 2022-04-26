load(file.path(path, "data", "names.RDs"))

overall_performance_all <- foreach(id = 1:30, .combine = rbind) %do% {
  fname_model <- paste0("trashcan", id, ".RDs")
  load(file = file.path(path, 'output', 'models', fname_model))
  
  fname_testset <- paste0("pitcher_test", id, ".Rds")
  load(file = file.path(path, 'output', 'pitchers', fname_testset))
  
  fname_factor <- paste0("pitcher_factor", id, ".RDs")
  load(file = file.path(path, 'output', 'pitchers', fname_factor))
  
  pitcher_first <- names$first_name[id]
  pitcher_last <- names$last_name[id]
  
  # out of sample performance (osp)
  # re-factor to get most thrown pitch first in each table
  pitcher_test$pitch_type <- factor(pitcher_test$pitch_type, factor_types)
  pitcher_osp <- pitcher_test %>% 
    mutate(sitONE_baseline = factor_types[1], 
           pitchhat_trashcan = predict(trashcan, pitcher_test))
  pitcher_osp$pitchhat_trashcan = factor(pitcher_osp$pitchhat_trashcan, factor_types)
  pitcher_osp <- pitcher_osp %>% 
    mutate(success_sitONE = ifelse(pitch_type == sitONE_baseline, 1, 0), 
           success_trashcan = ifelse(pitch_type == pitchhat_trashcan, 1, 0))
  
  # title strings for kable tables 
  op <- paste("Overall Performance:", pitcher_first, pitcher_last)
  bp <- paste("By Pitch Performance:", pitcher_first, pitcher_last)
  
  # see how the models do overall
  # comparing to a "sit FF" strategy of only guessing fastball
  # if a pitches does not throw a FF, change to baseline of...
  # guessing most thrown pitch
  overall_performance <- pitcher_osp %>% 
    summarise(rate_sitONE = round(sum(success_sitONE)/length(pitch_type), 3), 
              rate_trashcan = round(sum(success_trashcan)/length(pitch_type),3))
  
  # see how the models do at classifying the different pitch types
  by_pitch_performance <- pitcher_osp %>% 
    group_by(pitch_type) %>% 
    summarise(rate_trashcan = round(sum(success_trashcan)/length(pitch_type),3)) %>% 
    arrange(pitch_type) %>% 
    kable(caption = bp)
  
  # create strings for naming tables according to loop index
  bypitch <- paste("by_pitch_performance", pitcher_first, pitcher_last, sep = "_")
  overall <- paste("overall_performance", id, sep = "_")
  
  # name table outputs according to loop index
  assign(overall, overall_performance)
  assign(bypitch, by_pitch_performance)
  
  # make filenames according to loop index
  fname_bypitch <- paste0("by_pitch_performance", id, ".RDs")
  
  # save table outputs according to loop index
  save(file = file.path(path, 'output', 'tables', fname_bypitch), list = bypitch)
  
  cbind(pitcher_last, pitcher_first, overall_performance)
} %>%
  arrange(desc(rate_trashcan)) %>% 
  kable(caption = "Overall Performance")

save(file = file.path(path, 'output', 'tables', "overall_performance_all.RDs"), list = 'overall_performance_all')
