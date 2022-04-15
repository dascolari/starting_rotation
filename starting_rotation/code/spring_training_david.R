# spring training tryouts - david
pitches <- read.csv(file.path(path, 'data', 'raw', 'pitches', 'pitches.csv'))
atbats <- read.csv(file.path(path, 'data', 'raw', 'archive', 'atbats.csv'))
names <- read_xlsx(file.path(path, 'data', 'raw', 'archive', 'first_lineup.xlsx'))
pitches <- merge(pitches, atbats, by = "ab_id") 
pitches <- merge(pitches, names, by.x = "pitcher_id", by.y = "id")


###########################
# analyzing clayton pitcher 
##########################

foreach(id = 1:30) %do% {
  pitcher <- pitches %>% 
    filter(pitcher_id == names$id[id] & is.na(zone) == F) %>% # Clayton pitcher
    mutate(first_pitch = ifelse(pitch_num == 1, 1, 0), 
           pitch_type = ifelse(pitch_type == "PO" | pitch_type == "" | pitch_type == "IN", "other", pitch_type), 
           prev_pitch = ifelse(first_pitch == 0, lag(pitch_type), "none"), 
           zone = factor(zone),
           prev_zone = factor(ifelse(first_pitch == 0, lag(zone), 15))) %>% 
    dummy_cols(select_columns = c("prev_pitch")) %>% 
    dplyr::select(first_pitch, starts_with("prev_pitch"), pitch_type, pitch_num, b_count, s_count, stand, starts_with("on_"), outs, p_score, b_score, inning, zone)
  pitcher_first <- names$first_name[id]
  pitcher_last <- names$last_name[id]
  
  ####################
  # "stuff" breakdown
  # count and proportion for each pitch type
  # an interesting table in and of itself
  # also allows us to set factor order of pitch type...
  # ... in order of most used
  ######################
  pt <- paste("Pitch Types:", pitcher_first, pitcher_last)
  pz <- paste("Pitch Zones", pitcher_first, pitcher_last)
  
  types <- pitcher %>% 
    group_by(pitch_type) %>% 
    summarise(count = length(pitch_type), .groups = 'drop') %>% 
    mutate(pct = round(count/sum(count), 3), 
           other_last = ifelse(pitch_type == "other", 0, 1)) %>% 
    arrange(desc(other_last), desc(count)) %>% 
    dplyr::select(- other_last) 
  
  zones <- pitcher %>% 
    group_by(zone) %>% 
    summarise(count = length(zone), .groups = 'drop') %>% 
    mutate(pct = round(count/sum(count), 3))
  
  table_types <- types %>% 
    kable(caption = pt)
  
  table_zones <- zones %>% 
    kable(caption = pz)
  
  # preserve order of most used as a factor
  # set the factor levels of pitch_type equal to this order
  factor_types <- factor(types$pitch_type, types$pitch_type)
  pitcher$pitch_type <- factor(pitcher$pitch_type, factor_types)
  factor_zones <- factor(zones$zone, zones$zone)
  pitcher$zone <- factor(pitcher$zone, factor_zones)

  pitcher_split = initial_split(pitcher, prop = .8)
  pitcher_train = training(pitcher_split)
  pitcher_test = testing(pitcher_split)
  

  # add in more game situation indicators
  trashcan <- randomForest(pitch_type ~ . - zone,
                           data = pitcher_train, importance = TRUE, 
                           na.action = na.exclude)
  
  
  # out of sample performance (osp)
  pitcher_osp <- pitcher_test %>% 
    mutate(sitFF_baseline = factor_types[1], 
           pitchhat_trashcan = predict(trashcan, pitcher_test),
           success_sitFF = ifelse(pitch_type == sitFF_baseline, 1, 0), 
           success_trashcan = ifelse(pitch_type == pitchhat_trashcan, 1, 0))
  
  op <- paste("Overall Performance:", pitcher_first, pitcher_last)
  bp <- paste("By Pitch Performance:", pitcher_first, pitcher_last)
  # see how the models do overall
  # comparing to a "sit FF" strategy of only guessing fastball
  # if a pitches does not throw a FF, change to baseline of...
  # guessing most thrown pitch
  overall_performance <- pitcher_osp %>% 
    summarise(rate_sitFF = round(sum(success_sitFF)/length(pitch_type), 3), 
              rate_trashcan = round(sum(success_trashcan)/length(pitch_type),3)) %>% 
    kable(caption = op)
  
  # see how the models do at classifying the different pitch types
  by_pitch_performance <- pitcher_osp %>% 
    group_by(pitch_type) %>% 
    summarise(rate_sitFF = round(sum(success_sitFF)/length(pitch_type), 3), 
              rate_trashcan = round(sum(success_trashcan)/length(pitch_type),3)) %>% 
    arrange(pitch_type) %>% 
    kable(caption = bp)
  
  assign(paste("zones", pitcher_first, pitcher_last, sep = "_"), table_zones)
  assign(paste("types", pitcher_first, pitcher_last, sep = "_"), table_types)
  assign(paste("overall_performance", pitcher_first, pitcher_last, sep = "_"), overall_performance)
  assign(paste("by_pitch_performance", pitcher_first, pitcher_last, sep = "_"), by_pitch_performance)
}

# # a small exploration into pitch_type labeled as "other"
# # my conclusion: we should drop blanks and non-pitches
# other <- pitcher %>% filter(pitch_type == "other")

#######################
# pitch type modelling
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

# FOR NOW, COMMENT OUT STUFF THAT HAS TO DO WITH ZONE
                          # 
                          # pitcher_train <- pitcher_train %>% 
                          #   mutate(pitchhat_first = predict(first_tri, pitcher_train), 
                          #          pitchhat_count = predict(with_count, pitcher_train), 
                          #          pitchhat_trashcan = predict(trashcan, pitcher_train))
                          # 
                          # 
                          # pitcher_test <- pitcher_test %>% 
                          #   mutate(pitchhat_first = predict(first_tri, pitcher_test), 
                          #          pitchhat_count = predict(with_count, pitcher_test), 
                          #          pitchhat_trashcan = predict(trashcan, pitcher_test))
                          # 
                          # # only previous pitch type and first_pitch binary
                          # ft <- randomForest(zone ~ . - b_count - s_count - 
                          #                             stand - on_3b - on_2b - on_1b - 
                          #                             o - p_score - inning - zone - 
                          #                      pitchhat_count - pitchhat_trashcan,
                          #                           data = pitcher_train, importance = TRUE, 
                          #                           na.action = na.exclude)
                          # 
                          # # add in ball and strike count
                          # wc <- randomForest(zone ~ . - stand - 
                          #                              on_3b - on_2b - on_1b - 
                          #                              o - p_score - inning - zone - 
                          #                      pitchhat_first - pitchhat_trashcan,
                          #                            data = pitcher_train, importance = TRUE, 
                          #                            na.action = na.exclude)
                          # 
                          # # add in more game trashcan indicators
                          # sitch <- randomForest(zone ~ . - zone - 
                          #                         pitchhat_first - pitchhat_count,
                          #                           data = pitcher_train, importance = TRUE, 
                          #                           na.action = na.exclude)




save(file = file.path(path, "output", "pitcher_envi.RData"), 
           list = c("types", "zones", "overall_performance", "by_pitch_performance", "trashcan"))
