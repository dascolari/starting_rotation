load(file.path(path, "output", "pitches_import.RData"))

###########################
# analyzing each pitch pitcher 
##########################

foreach(id = 1:30) %do% {
  pitcher <- pitches %>% 
    filter(pitcher_id == names$id[id] & 
             is.na(zone) == FALSE &
             pitch_type != "PO" & 
             pitch_type != "" & 
             pitch_type != "IN" &
             pitch_type != "UN") %>% 
    mutate(first_pitch = ifelse(pitch_num == 1, 1, 0), 
           prev_pitch = ifelse(first_pitch == 0, lag(pitch_type), "none"), 
           zone = factor(zone),
           prev_zone = factor(ifelse(first_pitch == 0, lag(zone), 15))) %>% 
    filter(is.na(prev_pitch) == FALSE) %>% 
    dummy_cols(select_columns = c("prev_pitch")) %>% 
    arrange(ab_id, pitch_num) %>% 
    dplyr::select(first_pitch, 
                  starts_with("prev_pitch"), 
                  pitch_type, 
                  pitch_num, 
                  zone,
                  b_count, 
                  s_count, 
                  stand, 
                  starts_with("on_"), 
                  outs, 
                  p_score, 
                  b_score, 
                  inning)
  
  pitcher_first <- names$first_name[id]
  pitcher_last <- names$last_name[id]
  
  # pitcher_first
  # pitcher_last
  
  ####################
  # "stuff" breakdown
  # count and proportion for each pitch type
  # an interesting table in and of itself
  # also allows us to set factor order of pitch type...
  # ... in order of most used
  ######################
  types <- pitcher %>% 
    group_by(pitch_type) %>% 
    summarise(count = length(pitch_type), .groups = 'drop') %>% 
    mutate(pct = round(count/sum(count), 3)) %>% 
    arrange(desc(count))
  
  zones <- pitcher %>% 
    group_by(zone) %>% 
    summarise(count = length(zone), .groups = 'drop') %>% 
    mutate(pct = round(count/sum(count), 3))
  
  #names for kable tables
  pt <- paste("Pitch Types:", pitcher_first, pitcher_last)
  pz <- paste("Pitch Zones", pitcher_first, pitcher_last)
  
  table_types <- types %>% 
    kable(caption = pt)
  
  table_zones <- zones %>% 
    kable(caption = pz)
  
  # preserve order of most used as a factor
  # set the factor levels of zone equal to this order
  # will use this order for pitch type tables later on
  factor_types <- factor(types$pitch_type, types$pitch_type)
  factor_zones <- factor(zones$zone, zones$zone)
  pitcher$zone <- factor(pitcher$zone, factor_zones)
  
  # getting rid of one pitch wonders
  pitcher <- merge(pitcher, types, by = "pitch_type") %>% 
    dplyr::select(-pct) %>% 
    filter(count > 1) %>% 
    dplyr::select(-count)
  
#### THE OLD TRAIN TEST SPLIT THE CREATES EMPTY CLASSES IN Y(our mom's p****) 
  # pitcher_split = initial_split(pitcher, prop = .8)
  # pitcher_train = training(pitcher_split)
  # pitcher_test = testing(pitcher_split)
  
  # train test split a different way
  pitcher_split <- partition(data = pitcher, p = .8, cat_col = "pitch_type")
  pitcher_train <- pitcher_split[[1]]
  pitcher_test <- pitcher_split[[2]]
  
  # re-factor like a little b****
  pitcher_train$pitch_type <- factor(pitcher_train$pitch_type)
  pitcher_test$pitch_type <- factor(pitcher_test$pitch_type)
  
  # holy shit please fucking work
  trashcan <- randomForest(pitch_type ~ . - zone,
                           data = pitcher_train, importance = TRUE, 
                           na.action = na.exclude)
  
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
              rate_trashcan = round(sum(success_trashcan)/length(pitch_type),3)) %>% 
    kable(caption = op)
  
  # see how the models do at classifying the different pitch types
  by_pitch_performance <- pitcher_osp %>% 
    group_by(pitch_type) %>% 
    summarise(rate_trashcan = round(sum(success_trashcan)/length(pitch_type),3)) %>% 
    arrange(pitch_type) %>% 
    kable(caption = bp)
  
  # name table outputs according to index
  assign(paste("zones", pitcher_first, pitcher_last, sep = "_"), table_zones)
  assign(paste("types", pitcher_first, pitcher_last, sep = "_"), table_types)
  assign(paste("overall_performance", pitcher_first, pitcher_last, sep = "_"), overall_performance)
  assign(paste("by_pitch_performance", pitcher_first, pitcher_last, sep = "_"), by_pitch_performance)
}

#######################
# tree graph
#
#######################

pitcher.tree = rpart(pitch_type ~ b_score + b_count + s_count + outs + pitch_num + on_1b + on_2b + on_3b + first_pitch + prev_pitch, 
                     data=pitcher_train,
                     control = rpart.control(cp = .005, minsplit = 30))
rpart.plot(pitcher.tree, cex = .7, type=2)
prune_1se = function(my_tree) {
  out = as.data.frame(my_tree$cptable)
  thresh = min(out$xerror + out$xstd)
  cp_opt = max(out$CP[out$xerror <= thresh])
  prune(my_tree, cp=cp_opt)
}

pitcher.tree_prune = prune_1se(pitcher.tree)

rpart.plot(pitcher.tree_prune, type=4, extra=1)



## some importance plots just to see
# varImpPlot(first_tri)
# varImpPlot(with_count)
# varImpPlot(trashcan)



save(file = file.path(path, "output", "pitcher_envi.RData"), 
           list = c("types", "zones", "overall_performance", "by_pitch_performance", "trashcan"))