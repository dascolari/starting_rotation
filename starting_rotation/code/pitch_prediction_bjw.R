library(tidyverse)
library(fastDummies)
library(rsample)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

pitches <- read_csv("R/mlb_pitch_data/pitches.csv")
atbats <- read_csv("R/mlb_pitch_data/atbats.csv")

pitches <- pitches %>% 
  mutate(first_pitch = ifelse(pitch_num == 1, 1, 0), 
         prev_pitch = ifelse(first_pitch == 0, lag(pitch_type), "none")) %>% 
  dummy_cols(select_columns = "prev_pitch")

pitches2015 <- pitches %>% 
  filter(ab_id < 2016000000) 

pitchesabs2015 = merge(x = pitches2015, y = atbats)

## Enter pitcher ID here
# Arrieta 453562

pitches_pitcher_2015 <- pitchesabs2015 %>% 
  filter(pitcher_id == 453562)

pitches_pitcher_2015$pitch_type <- as.factor(pitches_pitcher_2015$pitch_type)


## Train / Test

pitcher_split =  initial_split(pitches_pitcher_2015, prop=0.8)
pitcher_train = training(pitcher_split)
pitcher_test  = testing(pitcher_split)

## Single tree and 1SE Prune

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

pitcher_test = pitcher_test %>% 
  mutate(pitch_pred = predict(pitcher.tree, pitcher_test, type='class'))

# Make a table of classification errors
xtabs(~pitch_type + pitch_pred, data=pitcher_test)

# Performance
nrow(pitcher_test)
(xtabs(~pitch_type + pitch_pred, data=pitcher_test) %>% diag %>% sum)/nrow(pitcher_test)

# For Arrieta, this was just barely better than predicting he throws slider every time

pitch.forest = randomForest(pitch_type ~ b_score + b_count + s_count + outs + pitch_num + on_1b + on_2b + on_3b + first_pitch + prev_pitch + prev_pitch_AB + prev_pitch_CH + prev_pitch_CU + prev_pitch_EP + prev_pitch_FA + prev_pitch_FC + prev_pitch_FF, 
                            data=pitcher_train, na.action=na.omit)

pitcher_test = pitcher_test %>% 
  mutate(pitch_pred = predict(pitch.forest, pitcher_test, type='class'))

# Make a table of classification errors
xtabs(~pitch_type + pitch_pred, data=pitcher_test)

# Performance
nrow(pitcher_test)
(xtabs(~pitch_type + pitch_pred, data=pitcher_test) %>% diag %>% sum)/nrow(pitcher_test)

# a marginal improvement


###
# Redo with a different pitcher
###

pitches2015 <- pitches %>% 
  filter(ab_id < 2016000000) 

pitchesabs2015 = merge(x = pitches2015, y = atbats)

## Enter pitcher ID here

pitches_pitcher_2015 <- pitchesabs2015 %>% 
  filter(pitcher_id == 477132)

pitches_pitcher_2015$pitch_type <- as.factor(pitches_pitcher_2015$pitch_type) 


## Train / Test

pitcher_split =  initial_split(pitches_pitcher_2015, prop=0.8)
pitcher_train = training(pitcher_split)
pitcher_test  = testing(pitcher_split)

pitcher_train$pitch_type <- droplevels(pitcher_train$pitch_type)

## Single tree and 1SE Prune

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

pitcher_test = pitcher_test %>% 
  mutate(pitch_pred = predict(pitcher.tree, pitcher_test, type='class'))

# Make a table of classification errors
xtabs(~pitch_type + pitch_pred, data=pitcher_test)

# Performance
nrow(pitcher_test)
(xtabs(~pitch_type + pitch_pred, data=pitcher_test) %>% diag %>% sum)/nrow(pitcher_test)

# For Kershaw, this is better than Arrieta

pitch.forest = randomForest(pitch_type ~ b_score + b_count + s_count + outs + pitch_num + on_1b + on_2b + on_3b + first_pitch + prev_pitch + prev_pitch_AB + prev_pitch_CH + prev_pitch_CU + prev_pitch_EP + prev_pitch_FA + prev_pitch_FC + prev_pitch_FF, 
                            data=pitcher_train, na.action=na.omit)

pitcher_test = pitcher_test %>% 
  mutate(pitch_pred = predict(pitch.forest, pitcher_test, type='class'))

# Make a table of classification errors
xtabs(~pitch_type + pitch_pred, data=pitcher_test)

# Performance
nrow(pitcher_test)
(xtabs(~pitch_type + pitch_pred, data=pitcher_test) %>% diag %>% sum)/nrow(pitcher_test)

# For Kershaw, we are just about as good as CART, but right only about 50% of the time

###############

## KNN

# Scale and center (?) the data

pitches_scale = pitchesabs2015 %>%
  select(pitch_type, b_score, b_count, s_count, outs, pitch_num, on_1b, on_2b, on_3b, first_pitch, prev_pitch_AB, prev_pitch_CH, prev_pitch_CU, prev_pitch_EP, prev_pitch_FA, prev_pitch_FC, prev_pitch_FF, pitcher_id) %>% 
  mutate(across(!c(pitch_type, pitcher_id), scale))


# Enter pitcher ID here

pitches_pitcher_2015 <- pitches_scale %>% 
  filter(pitcher_id == 453562)

pitches_pitcher_2015$pitch_type <- as.factor(pitches_pitcher_2015$pitch_type) 

# Train / Test Split

pitcher_split =  initial_split(pitches_pitcher_2015, prop=0.8)
pitcher_train = training(pitcher_split)
pitcher_test  = testing(pitcher_split)

knn_k5 = knnreg(pitch_type ~ b_score + b_count + s_count + outs + pitch_num + on_1b + on_2b + on_3b + first_pitch + prev_pitch_CH + prev_pitch_CU + prev_pitch_FF,
                data=pitcher_train, k=5)
knn_k15 = knnreg(pitch_type ~ b_score + b_count + s_count + outs + pitch_num + on_1b + on_2b + on_3b + first_pitch + prev_pitch_CH + prev_pitch_CU + prev_pitch_FF,
                data=pitcher_train, k=15)
knn_k25 = knnreg(pitch_type ~ b_score + b_count + s_count + outs + pitch_num + on_1b + on_2b + on_3b + first_pitch + prev_pitch_CH + prev_pitch_CU + prev_pitch_FF,
                data=pitcher_train, k=25)

# put the data and predictions in a single data frame
pitcher_test = pitcher_test %>%
  mutate(pitch_pred_K5 = predict(knn_k5, pitcher_test, type = 'class'),
         pitch_pred_K15 = predict(knn_k15, pitcher_test, type = 'class'),
         pitch_pred_K25 = predict(knn_k25, pitcher_test, type = 'class'))

# Make a table of classification errors
xtabs(~pitch_type + pitch_pred_K5, data=pitcher_test)
xtabs(~pitch_type + pitch_pred_K15, data=pitcher_test)
xtabs(~pitch_type + pitch_pred_K25, data=pitcher_test)

# performance:
nrow(pitcher_test)
xtabs(~pitch_type + pitch_pred_K5, data=pitcher_test) %>% diag %>% sum
xtabs(~pitch_type + pitch_pred_K15, data=pitcher_test) %>% diag %>% sum
xtabs(~pitch_type + pitch_pred_K25, data=pitcher_test) %>% diag %>% sum
