library(caret)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(mosaic)
library(rsample)
library(modelr)
library(randomForest)
library(gbm)
library(foreach)
library(pitchRx)
library(LICORS)

pitch_ab_2015 <- read.csv("~/School/University of Texas-Austin/Classes/Data Mining/pitch_ab_2015.csv")


test_arrieta = pitch_ab_2015 %>%
  filter(pitcher_id == 453562)


test_arrieta <- test_arrieta %>% 
  mutate(first_pitch = ifelse(pitch_num == 1, 1, 0), 
         prev_pitch = ifelse(first_pitch == 0, lag(pitch_type), "none")) %>%
  filter(pitch_type != "")%>%
  select(pitch_type, b_score, b_count, s_count, outs, pitch_num, on_1b, on_2b, on_3b, p_score, p_throws, stand, prev_pitch, first_pitch, px, pz)

test_arrieta$pitch_type = factor(test_arrieta$pitch_type)
test_arrieta$prev_pitch = factor(test_arrieta$prev_pitch)
# test_arrieta$stand = factor(test_arrieta$stand)

test_split = initial_split(test_arrieta, 0.8)
test_train = training(test_split)
test_test = testing(test_split)

# prune_1se = function(my_tree) {
#   out = as.data.frame(my_tree$cptable)
#   thresh = min(out$xerror + out$xstd)
#   cp_opt = max(out$CP[out$xerror <= thresh])
#   prune(my_tree, cp=cp_opt)
# }


# Predict first pitch with overall career metrics and batter metrics
# Then we can use lag pitches for later pitches

# bball_cart_folds = crossv_kfold(test, k=5)

# cart_models = map(bball_cart_folds$train, ~prune_1se(rpart(pitch_type ~ prev_pitch,
#                                                         data =., 
#                                                         control = rpart.control(cp = .0001, minsplit = 10))))
# cart_errs = map2_dbl(cart_models, ca_cart_folds$test, modelr::rmse)
# cart_errs = mean(cart_errs)

# first_try = rpart(pz ~ pitch_type,
#                        data =test_train, 
#                        control = rpart.control(cp = .0001, minsplit = 10))

# rpart.plot(prune_1se(first_try), type=4, extra=1)

# rmse(prune_1se(first_try), test_test)

pitch_count = test_test %>%
  group_by(pitch_type)%>%
  summarize(count = n())



first_try = randomForest(pitch_type ~ .-px-pz,
                  data =test_train, 
                  importance = TRUE)


test_train = test_train %>%
  mutate(pred_pitch = predict(first_try, test_train))


px_model = randomForest(px~.-pz-pitch_type,
                        data=test_train,
                        importance = TRUE)

test_train = test_train %>%
  mutate(pred_px = predict(px_model, test_train))

pred_pitch = as.data.frame(predict(first_try, test_train))
pred_pitch = pred_pitch %>%
  mutate(actual = test_train$pitch_type)

pz_model = randomForest(pz~.-px-pitch_type,
                        data = test_train,
                        importance = TRUE)

test_train = test_train %>%
  mutate(pred_pz = predict(pz_model, test_train))

test_test = test_test %>%
  mutate(pred_pitch = predict(first_try, test_test))


test_test = test_test %>%
  mutate(pred_px = predict(px_model, test_test))

test_test = test_test %>%
  mutate(pred_pz = predict(pz_model, test_test))



# Make a table of classification errors
xtabs(~pitch_type + pred_pitch, data=test_test)

# Performance
nrow(test_test)
(xtabs(~pitch_type + pred_pitch, data=test_test) %>% diag %>% sum)/nrow(test_test)

# q_hat = as.data.frame(predict(second_try, test_test))
# y_hat = as.data.frame(y_hat)
# q_hat = as.data.frame(y_hat)
# yhat_test = as.data.frame(ifelse(y_hat == pmax(y_hat$CU, y_hat$CH,
#                                  y_hat$IN, y_hat$FF,
#                                  y_hat$SI, y_hat$SL), 1, 0))

# pred_pitch_count = y_hat %>%
#   group_by(y_hat)%>%
#   summarize(count = n())


# sum(yhat_test$CU)

# tab <- tibble::as_tibble(yhat_test)

# transform to long format the dummy columns
# tab_long <- tidyr::pivot_longer(tab, 
#                                cols = tidyselect::contains("."),
#                                 names_to = c("groups", "levels"),
#                                 names_pattern = "^([^.]*)[.](.*)")




# groups <- unique(tab_long$groups)
# keep only non dummy value and do not keep temp value col
# tab_filter <- dplyr::select(
#  dplyr::filter(tab_long, value == 1),
#  -value)
# tranform to wide format   
# tab_wide <- tidyr::pivot_wider(
#   tab_filter,
#   names_from = groups, 
#   values_from = levels)
# convert to factors the groups column
# new_tab <- dplyr::mutate_at(
#  tab_wide,
#   groups,
#   ~ forcats::as_factor(.)
# )
# dplyr::glimpse(new_tab)

# sum(yhat_test$CU)


# Player Search

player_names <- read.csv("~/School/University of Texas-Austin/Classes/Data Mining/starting_rotation/starting_rotation/data/raw/archive/archive/player_names.csv")

ide = ifelse(player_names$first_name == "Chad" & 
               player_names$last_name =="Bettis",
             player_names$id,0)
sum(ide)

name_list = c(453562,506433,519455,518516,519242,446372,456034,
              544931,453286,433587,477132,594798,502042,425844,
              453343,572096,547973,502202,434718,572971,474521,
              502154,543521,543243,451584,453192,502381,543037,
              608379,518452)

starters = filter(player_names, id %in% name_list)

starters = starters %>%
  mutate(name = paste(first_name,last_name,sep=" "))

test_list = list(572096,433587,502042,502154,451584,572971,446372,
                 502381,456034,453286,547973,477132,518516,543243,
                 434718,502202,425844,474521,519455,594798,543037,
                 453343,453562,453192,544931,543521,608379,519242,
                 518452,506433)

names(test_list) = c(starters[,4])

path = "C:/Users/Student/Documents/School/University of Texas-Austin/Classes/Data Mining/starting_rotation/starting_rotation"

test = pitch_ab_2015 %>%
  filter(pitcher_id == 506433)

as.data.frame(types_Jake_Arrieta)


write.csv(starters, "first_lineup.csv", row.names = FALSE)

working = pitch_ab_2015 %>%
  mutate(first_pitch = ifelse(pitch_num == 1, 1, 0), 
       prev_pitch = ifelse(first_pitch == 0, lag(pitch_type), "none")) %>%
  filter(pitch_type != "" | pitch_type != " ")%>%
  select(pitcher_id, pitch_type, b_score, b_count, s_count, outs, pitch_num, on_1b, on_2b, on_3b, p_score, p_throws, stand, prev_pitch, first_pitch, px, pz)

working$pitch_type = factor(working$pitch_type)
working$prev_pitch = factor(working$prev_pitch)

for (i in 1:30) {
  what = as.numeric(starters$id[i])
  test = working %>%
    filter(pitcher_id == what)
}


#swing_at_random = foreach(i = starters, .combine='rbind') %dopar% {
  working <- working %>% 
    filter(pitcher_id == starters$id[15])
  
  #pitches_pitcher_2015$pitch_type <- na.omit(pitches_pitcher_2015$pitch_type)
  
  #pitches_pitcher_2015$pitch_type <- as.factor(pitches_pitcher_2015$pitch_type)
  
  ## Train / Test
  
  pitcher_split =  initial_split(working, prop=0.8)
  pitcher_train = training(pitcher_split)
  pitcher_test  = testing(pitcher_split)
  
  ## Random Forest
  
  pitch.forest = randomForest(pitch_type ~ ., 
                              data=pitcher_train, na.action=na.omit)
  
  pitcher_test = pitcher_test %>% 
    mutate(pitch_pred = predict(pitch.forest, pitcher_test, type='class'))
  
  # Make a table of classification errors
  xtabs(~pitch_type + pitch_pred, data=pitcher_test)
  
  # Performance
  nrow(pitcher_test)
  hit_rate = (xtabs(~pitch_type + pitch_pred, data=pitcher_test) %>% diag %>% sum)/nrow(pitcher_test)
  
#  c(id = i, rforest = hit_rate)
#} %>% as.data.frame

performance = merge(x = swing_at_random, y = swing_naively)
performance = merge(x = performance, y = player_names)



#### Analyzing Different pitches

pitches_2015 <- read.csv("~/School/University of Texas-Austin/Classes/Data Mining/pitches_2015.csv")



plot_data = pitches %>%
  filter(pitcher_id == 425844)%>%
  filter(pitch_type != "") %>%
  filter(pitch_type != "IN")%>%
  select(pitch_type,px,pz,start_speed,end_speed,spin_rate,spin_dir,break_angle,break_length,break_y)

plot_data$pitch_type = as.factor(plot_data$pitch_type)

ggplot(plot_data)+
  geom_point(aes(x=spin_rate,y=spin_dir,color=pitch_type),alpha=.2)

ggplot(plot_data)+
  geom_point(aes(x=break_angle,y=break_length,color=pitch_type),alpha=.2)

ggplot(plot_data)+
  geom_density(aes(x=break_length))+
  facet_wrap(~pitch_type)

ggplot(plot_data)+
  geom_point(aes(x=px, y=pz, color = pitch_type))+
  geom_segment(aes(x = -2.66, y = 1.583087, xend = 2.66, yend = 1.583087))+
  geom_segment(aes(x = -2.66, y = 3.458079, xend = 2.66, yend = 3.458079))+
  geom_segment(aes(x = 2, y = 1.583087, xend = 2.66, yend = 3.458079))+
  geom_segment(aes(x = -2.66 , y = 1.583087, xend = -2.66, yend = 3.458079))

mean(pitch_ab_2015$sz_bot)

table(pitch_sum)






X = test[,(1:7)]
X = scale(X, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")

# K-means++
clust_kplus = kmeanspp(X, k=4, nstart=50)

ggplot(test)+
  geom_point(aes(x=break_angle,y=break_length,color=clust_kplus$cluster),alpha=.2)


###### PCA

pc_pitch = prcomp(test[,-1], rank=6, scale=TRUE)

summary(pc_pitch)

test = test %>%
  rownames_to_column('pitch_id')

pitch_x = as.data.frame(pc_pitch$x)%>%
  rownames_to_column('pitch_id')

pc_data = merge(test, pitch_x, by='pitch_id')%>%
  select(!pitch_id)


ggplot(pc_data)+
  geom_point(aes(x=pitch_type,y=PC1))

ggplot(pc_data)+
  geom_point(aes(x=break_y, y= PC1, color=pitch_type))


pitch_sum = pitches %>%
  filter(pitch_type != "") %>%
  filter(pitch_type != "IN")%>%
  select(pitch_type,px,pz,start_speed,end_speed,spin_rate,spin_dir,break_angle,break_length,break_y)%>%
  mutate(pitch_type = as.factor(pitch_type))%>%
  group_by(pitch_type)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

sum_ff_si = pitch_ab_2015 %>%
  filter(pitcher_id == 453562)%>%
  group_by(pitch_type)%>%
  summarize(break_y = mean(break_y), spin_rate = mean(spin_rate),
            spin_dir = mean(spin_dir), start_speed = mean(start_speed),
            end_speed = mean(end_speed), px = mean(px), pz = mean(pz))




#### Code for Situation

load("C:/Users/Student/Documents/School/University of Texas-Austin/Classes/Data Mining/starting_rotation/starting_rotation/output/pitches_import.RData")


by_pitch_performance_Trevor_Rosenthal


player_names <- read.csv(file.path(path, "data","raw","archive","archive","player_names.csv"))


#### Markdown Code

library(lemon)
knit_print.data.frame <- lemon_print

harrison_path = "C:/Users/Student/Documents/School/University of Texas-Austin/Classes/Data Mining/starting_rotation/starting_rotation"

load(file.path(path, "output", "pitches_import.RData"))

pitch_sum = pitches %>%
  filter(pitch_type != "") %>%
  filter(pitch_type != "IN")%>%
  dplyr::select(pitch_type,px,pz,start_speed,end_speed,spin_rate,spin_dir,break_angle,break_length,break_y)%>%
  mutate(pitch_type = as.factor(pitch_type))%>%
  group_by(pitch_type)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

pitch_sum = pitch_sum %>%
  mutate_if(is.numeric,round, digits=3)

plot_data = pitches %>%
  filter(pitch_type != "") %>%
  filter(pitch_type != "IN")%>%
  filter(pitch_type != "UN")%>%
  filter(pitch_type != "FA")%>%
  dplyr::select(pitcher_id,pitch_type,px,pz,start_speed,end_speed,spin_rate,spin_dir,break_angle,break_length,break_y)

plot_data$pitch_type = as.factor(plot_data$pitch_type)

plot_archer = plot_data%>%
  filter(pitcher_id == 502042)

plot_arrieta = plot_data %>%
  filter(pitcher_id == 453562)

ggplot(plot_archer)+
  geom_point(aes(x=spin_rate,y=spin_dir,color=pitch_type),alpha=.2)

ggplot(plot_archer)+
  geom_point(aes(x=break_angle,y=break_length,color=pitch_type),alpha=.2)

ggplot(plot_arrieta)+
  geom_point(aes(x=spin_rate,y=spin_dir,color=pitch_type),alpha=.2)+
  ggtitle("Spin Rate and Direction for Jake Arrieta's Pitches")

ggplot(plot_arrieta)+
  geom_point(aes(x=break_angle,y=break_length,color=pitch_type),alpha=.2)+
  ggtitle("Break Angle and Length for Jake Arrieta's Pitches")


plot_all = plot_data%>%
  filter(pitcher_id %in% c(502042,453562))

ggplot(plot_all)+
  geom_point(aes(x=break_angle,y=break_length,color=pitch_type),alpha=.2)

ggplot(plot_data)+
  geom_density(aes(x=start_speed), fill="blue")+
  ggtitle("Distribution of Start Speeds across Pitches")+
  facet_wrap(~pitch_type)

ggplot(plot_data)+
  geom_density(aes(x=break_length), fill="red")+
  ggtitle("Distribution of Break Length across Pitches")+
  facet_wrap(~pitch_type)












