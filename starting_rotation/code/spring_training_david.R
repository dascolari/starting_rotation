# spring training tryouts - david
pitches <- read.csv(file.path(path, 'data', 'raw', 'pitches', 'pitches.csv'))
atbats <- read.csv(file.path(path, 'data', 'raw', 'archive', 'atbats.csv'))
pitches <- merge(pitches, atbats) 

###########################
# analyzing clayton kershaw 
##########################
kershaw <- pitches %>% 
  filter(pitcher_id == 477132) %>% # Clayton Kershaw
  mutate(first_pitch = ifelse(pitch_num == 1, 1, 0), 
         pitch_type = ifelse(pitch_type == "" | pitch_type == "IN", "other", pitch_type), 
         prev_pitch = ifelse(first_pitch == 0, lag(pitch_type), "none")) %>% 
  dummy_cols(select_columns = "prev_pitch") %>% 
  dplyr::select(first_pitch, starts_with("prev_pitch"), pitch_type, b_count, s_count, stand, starts_with("on_"), o, p_score, inning, everything())

# # a small exploration into pitch_type labeled as "other"
# # my conclusion: we should drop blanks and non-pitches
# other <- kershaw %>% filter(pitch_type == "other")

####################
# "stuff" breakdown
# count and proportion for each pitch type
# an interesting table in and of itself
# also allows us to set factor order of pitch type...
# ... in order of most used
######################

types <- kershaw %>% 
  group_by(pitch_type) %>% 
  summarise(count = length(pitch_type), .groups = 'drop') %>% 
  mutate(pct = round(count/sum(count), 3), 
         other_last = ifelse(pitch_type == "other", 0, 1)) %>% 
  arrange(desc(other_last), desc(count)) %>% 
  dplyr::select(- other_last)

# preserve order of most used as a factor
# set the factor levels of pitch_type equal to this order
factor_types <- factor(types$pitch_type, types$pitch_type)
kershaw$pitch_type <- factor(kershaw$pitch_type, factor_types)


#######################
# pitch type modelling
#
#######################
kershaw_split = initial_split(kershaw, prop = .8)
kershaw_train = training(kershaw_split)
kershaw_test = testing(kershaw_split)

# # use this tree for a poterntial graphic
# tree <- rpart(pitch_type ~ ., 
#               data = kershaw_train, 
#               na.action = na.exclude, 
#               control = rpart.control(cp = 0.00001))

# only previous pitch type and first_pitch binary
first_tri <- randomForest(pitch_type ~ . - b_count - s_count - 
                            stand - on_3b - on_2b - on_1b - 
                            o - p_score - inning,
             data = kershaw_train, importance = TRUE, 
             na.action = na.exclude)

# add in ball and strike count
with_count <- randomForest(pitch_type ~ . - stand - 
                             on_3b - on_2b - on_1b - 
                             o - p_score - inning,
                           data = kershaw_train, importance = TRUE, 
                           na.action = na.exclude)

# add in more game situation indicators
situation <- randomForest(pitch_type ~ .,
             data = kershaw_train, importance = TRUE, 
             na.action = na.exclude)

## some importance plots just to see
# varImpPlot(first_tri)
# varImpPlot(with_count)
# varImpPlot(situation)

# out of sample performance (osp)
kershaw_osp <- kershaw_test %>% 
  mutate(sitFF_baseline = factor_types[1], 
         pitchhat_first = predict(first_tri, kershaw_test), 
         pitchhat_count = predict(with_count, kershaw_test), 
         pitchhat_situation = predict(situation, kershaw_test), 
         success_sitFF = ifelse(pitch_type == sitFF_baseline, 1, 0), 
         success_first = ifelse(pitch_type == pitchhat_first, 1, 0), 
         success_count = ifelse(pitch_type == pitchhat_count, 1, 0), 
         success_situation = ifelse(pitch_type == pitchhat_situation, 1, 0))

# see how the models do overall
# comparing to a "sit FF" strategy of only guessing fastball
# if a pitches does not throw a FF, change to baseline of...
# guessing most thrown pitch
overall_performance <- kershaw_osp %>% 
  summarise(rate_sitFF = round(sum(success_sitFF)/length(pitch_type), 3), 
            rate_first = round(sum(success_first)/length(pitch_type), 3), 
            rate_count = round(sum(success_count)/length(pitch_type), 3),
            rate_situation = round(sum(success_situation)/length(pitch_type),3))

# see how the models do at classifying the different pitch types
by_pitch_performance <- kershaw_osp %>% 
  group_by(pitch_type) %>% 
  summarise(rate_sitFF = round(sum(success_sitFF)/length(pitch_type), 3), 
            rate_first = round(sum(success_first)/length(pitch_type), 3), 
            rate_count = round(sum(success_count)/length(pitch_type), 3),
            rate_situation = round(sum(success_situation)/length(pitch_type),3)) %>% 
  arrange(pitch_type)