library(tidyverse)
library(fastDummies)
library(rsample)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(naivebayes)
library(foreach)
library(readxl)

pitches <- read_csv("mlb_pitch_data/pitches.csv")
atbats <- read_csv("mlb_pitch_data/atbats.csv")
player_names <- read_csv("mlb_pitch_data/player_names.csv")
first_lineup <- read_excel("mlb_pitch_data/first_lineup.xlsx")

pitches <- pitches %>% 
  mutate(first_pitch = ifelse(pitch_num == 1, 1, 0), 
         prev_pitch = ifelse(first_pitch == 0, lag(pitch_type), "none")) %>% 
  dummy_cols(select_columns = "prev_pitch")

pitches2015 <- pitches %>% 
  filter(ab_id < 2016000000) 

pitchesabs2015 = merge(x = pitches2015, y = atbats)

######
#
######


## Enter pitcher ID here
# Arrieta 453562

pitches_pitcher_2015 <- pitchesabs2015 %>% 
  filter(pitcher_id == 572096)

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

# Samardzjia 502188 Kershaw 477132

pitches2015 <- pitches %>% 
  filter(ab_id < 2016000000) 

pitchesabs2015 = merge(x = pitches2015, y = atbats)

## Enter pitcher ID here

pitches_pitcher_2015 <- pitchesabs2015 %>% 
  filter(pitcher_id == 572096)

# pitches_pitcher_2015$pitch_type <- as.factor(pitches_pitcher_2015$pitch_type) 


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

pitch.forest = randomForest(pitch_type ~ b_score + b_count + s_count + outs + pitch_num + on_1b + on_2b + on_3b + first_pitch + prev_pitch_CH + prev_pitch_CU + prev_pitch_EP + prev_pitch_FA + prev_pitch_FC + prev_pitch_FF, 
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

## KNN - discontinued until it works

# # Scale and center (?) the data
# 
# pitches_scale = pitchesabs2015 %>%
#   select(pitch_type, b_score, b_count, s_count, outs, pitch_num, on_1b, on_2b, on_3b, first_pitch, prev_pitch_AB, prev_pitch_CH, prev_pitch_CU, prev_pitch_EP, prev_pitch_FA, prev_pitch_FC, prev_pitch_FF, pitcher_id) %>% 
#   mutate(across(!c(pitch_type, pitcher_id), scale))
# 
# 
# # Enter pitcher ID here
# 
# pitches_pitcher_2015 <- pitches_scale %>% 
#   filter(pitcher_id == 453562)
# 
# pitches_pitcher_2015$pitch_type <- as.factor(pitches_pitcher_2015$pitch_type) 
# 
# # Train / Test Split
# 
# pitcher_split =  initial_split(pitches_pitcher_2015, prop=0.8)
# pitcher_train = training(pitcher_split)
# pitcher_test  = testing(pitcher_split)
# 
# knn_k5 = knnreg(pitch_type ~ b_score + b_count + s_count + outs + pitch_num + on_1b + on_2b + on_3b + first_pitch + prev_pitch_CH + prev_pitch_CU + prev_pitch_FF,
#                 data=pitcher_train, k=5)
# knn_k15 = knnreg(pitch_type ~ b_score + b_count + s_count + outs + pitch_num + on_1b + on_2b + on_3b + first_pitch + prev_pitch_CH + prev_pitch_CU + prev_pitch_FF,
#                 data=pitcher_train, k=15)
# knn_k25 = knnreg(pitch_type ~ b_score + b_count + s_count + outs + pitch_num + on_1b + on_2b + on_3b + first_pitch + prev_pitch_CH + prev_pitch_CU + prev_pitch_FF,
#                 data=pitcher_train, k=25)
# 
# # put the data and predictions in a single data frame
# pitcher_test = pitcher_test %>%
#   mutate(pitch_pred_K5 = predict(knn_k5, pitcher_test, type = 'class'),
#          pitch_pred_K15 = predict(knn_k15, pitcher_test, type = 'class'),
#          pitch_pred_K25 = predict(knn_k25, pitcher_test, type = 'class'))
# 
# # Make a table of classification errors
# xtabs(~pitch_type + pitch_pred_K5, data=pitcher_test)
# xtabs(~pitch_type + pitch_pred_K15, data=pitcher_test)
# xtabs(~pitch_type + pitch_pred_K25, data=pitcher_test)
# 
# # performance:
# nrow(pitcher_test)
# xtabs(~pitch_type + pitch_pred_K5, data=pitcher_test) %>% diag %>% sum
# xtabs(~pitch_type + pitch_pred_K15, data=pitcher_test) %>% diag %>% sum
# xtabs(~pitch_type + pitch_pred_K25, data=pitcher_test) %>% diag %>% sum

###
# Naive Bayes
###


pitches2015 <- pitches %>% 
  filter(ab_id < 2016000000) 

pitchesabs2015 = merge(x = pitches2015, y = atbats)

## Enter pitcher ID here
# Arrieta 453562


pitches_pitcher_2015 <- pitchesabs2015 %>% 
  filter(pitcher_id == 453562) %>% 
  select(pitch_type, b_score, b_count, s_count, outs, pitch_num, on_1b, on_2b, on_3b, first_pitch, prev_pitch_AB, prev_pitch_CH, prev_pitch_CU, prev_pitch_EP, prev_pitch_FA, prev_pitch_FC, prev_pitch_FF, pitcher_id, inning, p_score)

X_NB = data.matrix(pitches_pitcher_2015)  # feature matrix
X_NB <- X_NB[, colnames(X_NB) != "pitch_type"]
y_NB = factor(pitches_pitcher_2015$pitch_type)

N = length(y_NB)
train_frac = 0.8
train_set = sample.int(N, floor(train_frac*N)) %>% sort
test_set = setdiff(1:N, train_set)

# training and testing matrices
X_train = X_NB[train_set,]
X_test = X_NB[test_set,]

# Training and testing response vectors
y_train = y_NB[train_set]
y_test = y_NB[test_set]

# Train the model
nb_model = multinomial_naive_bayes(x = X_train, y = y_train)

# predict on the test set
y_test_pred = predict(nb_model, X_test)

# look at the confusion matrix
table(y_test, y_test_pred)

# overall test-set accuracy
sum(diag(table(y_test, y_test_pred)))/length(y_test)

#####
# Analyze multiple pitchers with Naive Bayes
#####

i_pitchers = c(first_lineup$id)
  
swing_naively = foreach(i = i_pitchers, .combine='rbind') %dopar% {
  pitches_pitcher_2015 <- pitchesabs2015 %>% 
    filter(pitcher_id == i) %>% 
    select(pitch_type, b_score, b_count, s_count, outs, pitch_num, on_1b, on_2b, on_3b, first_pitch, prev_pitch_AB, prev_pitch_CH, prev_pitch_CU, prev_pitch_EP, prev_pitch_FA, prev_pitch_FC, prev_pitch_FF, pitcher_id, inning, p_score)
  
  X_NB = data.matrix(pitches_pitcher_2015)  # feature matrix
  X_NB <- X_NB[, colnames(X_NB) != "pitch_type"]
  y_NB = factor(pitches_pitcher_2015$pitch_type)
  
  N = length(y_NB)
  train_frac = 0.8
  train_set = sample.int(N, floor(train_frac*N)) %>% sort
  test_set = setdiff(1:N, train_set)
  
  # training and testing matrices
  X_train = X_NB[train_set,]
  X_test = X_NB[test_set,]
  
  # Training and testing response vectors
  y_train = y_NB[train_set]
  y_test = y_NB[test_set]
  
  # Train the model
  nb_model = multinomial_naive_bayes(x = X_train, y = y_train)
  
  # predict on the test set
  y_test_pred = predict(nb_model, X_test)
  
  # look at the confusion matrix
  table(y_test, y_test_pred)
  
  # overall test-set accuracy
  hit_rate = sum(diag(table(y_test, y_test_pred)))/length(y_test)
  
  c(id = i, naive = hit_rate)
} %>% as.data.frame

#####
# Multiple pitchers with Random Forest
#####

i_pitchers = c(first_lineup$id[1:15])

swing_at_random = foreach(i = i_pitchers, .combine='rbind') %dopar% {
  pitches_pitcher_2015 <- pitchesabs2015 %>% 
    filter(pitcher_id == i)
  
  pitches_pitcher_2015$pitch_type <- na.omit(pitches_pitcher_2015$pitch_type)
  
  pitches_pitcher_2015$pitch_type <- as.factor(pitches_pitcher_2015$pitch_type)
  
  ## Train / Test
  
  pitcher_split =  initial_split(pitches_pitcher_2015, prop=0.8)
  pitcher_train = training(pitcher_split)
  pitcher_test  = testing(pitcher_split)
  
  ## Random Forest
  
  pitch.forest = randomForest(pitch_type ~ b_score + b_count + s_count + outs + pitch_num + on_1b + on_2b + on_3b + first_pitch + prev_pitch + prev_pitch_AB + prev_pitch_CH + prev_pitch_CU + prev_pitch_EP + prev_pitch_FA + prev_pitch_FC + prev_pitch_FF, 
                              data=pitcher_train, na.action=na.omit)
  
  pitcher_test = pitcher_test %>% 
    mutate(pitch_pred = predict(pitch.forest, pitcher_test, type='class'))
  
  # Make a table of classification errors
  xtabs(~pitch_type + pitch_pred, data=pitcher_test)
  
  # Performance
  nrow(pitcher_test)
  hit_rate = (xtabs(~pitch_type + pitch_pred, data=pitcher_test) %>% diag %>% sum)/nrow(pitcher_test)
  
  c(id = i, rforest = hit_rate)
} %>% as.data.frame

performance = merge(x = swing_at_random, y = swing_naively)
performance = merge(x = performance, y = player_names)
