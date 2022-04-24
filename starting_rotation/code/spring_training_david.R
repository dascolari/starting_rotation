load(file.path(path, "output", "pitches_import.RData"))

###########################
# analyzing each pitch pitcher 
##########################

overall_performance_all <- foreach(id = 1:30, .combine = rbind) %do% {
  pitcher <- pitches %>% 
    filter(pitcher_id == names$id[id] & 
             is.na(zone) == FALSE &
             pitch_type != "PO" & 
             pitch_type != "" & 
             pitch_type != "IN" &
             pitch_type != "UN") %>% 
    arrange(ab_id, pitch_num) %>% 
    mutate(first_pitch = ifelse(pitch_num == 1, 1, 0), 
           prev_pitch = ifelse(first_pitch == 0, 
                               lag(pitch_type), "none"),
           prev_pitch2 = ifelse(first_pitch == 0 & lag(first_pitch) == 0, 
                                lag(pitch_type, n = 2L), "none"),
           # prev_pitch3 = ifelse(first_pitch == 0 & prev_pitch2 != "none" & prev_pitch != "none", 
           #                      lag(pitch_type, n = 3L), "none"),
           zone = factor(zone),
           prev_zone = factor(ifelse(first_pitch == 0, lag(zone), 15))) %>% 
    filter(is.na(prev_pitch) == FALSE) %>% 
    arrange(ab_id, pitch_num) %>% 
    dplyr::select(first_pitch, 
                  starts_with("prev_pitch"), 
                  pitch_type, 
                  pitch_num, 
                  zone,
                  ab_id,
                  g_id,
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
  
  fname_factor <- paste0("pitcher_factor", id, ".RDs")
  save(file = file.path(path, 'output', 'pitchers', fnames_factor))
  
  # getting rid of one pitch wonders
  pitcher <- merge(pitcher, types, by = "pitch_type") %>% 
    dplyr::select(-pct) %>% 
    filter(count > 1) %>% 
    dplyr::select(-count)
  
  # add on lagged events (result of the previous at bat)
  ab_events <- pitches %>% 
    filter(pitcher_id == names$id[id]) %>% 
    arrange(ab_id, pitch_num) %>% 
    group_by(ab_id) %>% 
    summarise(event = first(event), 
              inning = first(inning), 
              g_id = first(g_id), 
              .groups = 'drop') %>% 
    mutate(prev_event = ifelse(inning == lag(inning) & g_id == lag(g_id), lag(event), "none"), 
           prev_event = ifelse(is.na(prev_event) == TRUE, "none", prev_event)) %>% 
    dplyr::select(ab_id, starts_with("prev_"))
  
  pitcher <- merge(pitcher, ab_events, by = "ab_id")
  
  gameday_stuff <- pitches %>% 
    filter(pitcher_id == names$id[id] & 
             is.na(zone) == FALSE &
             pitch_type != "PO" & 
             pitch_type != "" & 
             pitch_type != "IN" &
             pitch_type != "UN" &
             inning <= 4) %>% 
    arrange(ab_id, pitch_num) %>% 
    group_by(g_id, pitch_type) %>% 
    summarise(count = length(pitch_type), .groups = 'drop') %>% 
    pivot_wider(id_cols = "g_id", names_from = "pitch_type", values_from = "count") %>% 
    replace(is.na(.), 0) %>% 
    mutate(total = rowSums(across(where(is.numeric))) - g_id) %>% 
    mutate_at(vars(- c("g_id", "total")), ~./total) %>% 
    mutate(inning = 5)
  
  max_inning <- max(pitcher$inning)
  
  foreach(i = 6:max_inning) %do% {
    inning_stuff <- gameday_stuff %>% mutate(inning = i)
    gameday_stuff <- rbind(gameday_stuff, inning_stuff)
  }
  
  pitcher <- merge(pitcher, gameday_stuff, by = c("g_id", "inning"), all.x = TRUE) %>% 
    replace(is.na(.), 0)
  
  
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
  trashcan <- randomForest(pitch_type ~ . - zone - ab_id - g_id,
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
              rate_trashcan = round(sum(success_trashcan)/length(pitch_type),3))
  
  # see how the models do at classifying the different pitch types
  by_pitch_performance <- pitcher_osp %>% 
    group_by(pitch_type) %>% 
    summarise(rate_trashcan = round(sum(success_trashcan)/length(pitch_type),3)) %>% 
    arrange(pitch_type) %>% 
    kable(caption = bp)
  
  # name table outputs according to index
  assign(paste("zones", pitcher_first, pitcher_last, sep = "_"), table_zones)
  assign(paste("types", pitcher_first, pitcher_last, sep = "_"), table_types)
  assign(paste("overall_performance", id, sep = "_"), overall_performance)
  assign(paste("by_pitch_performance", pitcher_first, pitcher_last, sep = "_"), by_pitch_performance)
  cbind(pitcher_last, pitcher_first, overall_performance)
} %>%
  arrange(desc(rate_trashcan))

varImpPlot(trashcan)

rpart.plot(pitcher.tree_prune, type=4, extra=1)

save.image(file = file.path(path, "output", "pitcher_envi.RData"))


#######################
# tree graph
#
#######################

# pitcher.tree = rpart(pitch_type ~ b_score + b_count + s_count + outs + pitch_num + on_1b + on_2b + on_3b + first_pitch + prev_pitch, 
#                      data=pitcher_train,
#                      control = rpart.control(cp = .005, minsplit = 30))
# rpart.plot(pitcher.tree, cex = .7, type=2)
# prune_1se = function(my_tree) {
#   out = as.data.frame(my_tree$cptable)
#   thresh = min(out$xerror + out$xstd)
#   cp_opt = max(out$CP[out$xerror <= thresh])
#   prune(my_tree, cp=cp_opt)
# }
# 
# pitcher.tree_prune = prune_1se(pitcher.tree)




###
## full inning history for each game
# max_inning <- max(pitcher$inning)
# gameday_stuff <- foreach(i = 2:max_inning, .combine = rbind) %do% {
#   pitches %>% 
#     filter(pitcher_id == names$id[id] & 
#              is.na(zone) == FALSE &
#              pitch_type != "PO" & 
#              pitch_type != "" & 
#              pitch_type != "IN" &
#              pitch_type != "UN" &
#              inning < i) %>% 
#     arrange(ab_id, pitch_num) %>% 
#     group_by(g_id, pitch_type) %>% 
#     summarise(count = length(pitch_type), .groups = 'drop') %>% 
#     pivot_wider(id_cols = "g_id", names_from = "pitch_type", values_from = "count") %>% 
#     replace(is.na(.), 0) %>% 
#     mutate(total = rowSums(across(where(is.numeric))) - g_id) %>% 
#     mutate_at(vars(- c("g_id", "total")), ~./total) %>% 
#     mutate(inning = i)
# } %>% 
#   arrange(g_id)
# 

# # game level pca
# gameday_stuff <- pitches %>% 
#   filter(pitcher_id == names$id[id] & 
#            is.na(zone) == FALSE &
#            pitch_type != "PO" & 
#            pitch_type != "" & 
#            pitch_type != "IN" &
#            pitch_type != "UN") %>% 
#   arrange(ab_id, pitch_num) %>% 
#   group_by(g_id, pitch_type) %>% 
#   summarise(count = length(pitch_type), .groups = 'drop') %>% 
#   pivot_wider(id_cols = "g_id", names_from = "pitch_type", values_from = "count") %>% 
#   replace(is.na(.), 0) %>% 
#   mutate(total = rowSums(across(where(is.numeric))) - g_id) %>% 
#   mutate_at(vars(- c("g_id", "total")), ~./total)
# 
# pcs <- prcomp(gameday_stuff, rank = 2, scale = TRUE)
# 
# summary(pcs)
# 
# pc_gameday <- pcs$x
# 
# gameday_stuff <- cbind(gameday_stuff, pc_gameday) %>% 
#   dplyr::select(g_id, PC1, PC2)
# 
# 
# pitcher <- merge(pitcher, gameday_stuff, by = c("g_id"), all.x = TRUE) %>% 
#   replace(is.na(.), 0)
# 
# foreach(i = 2:max_inning) %do% {
#   inning_stuff <- gameday_stuff %>% mutate(inning = i)
#   gameday_stuff <- rbind(gameday_stuff, inning_stuff)
# }


