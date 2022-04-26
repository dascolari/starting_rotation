foreach(id = 12:12) %do% {
  
  fname_pitcher <- paste0("pitcher", id, ".RDs")
  load(file = file.path(path, 'output', 'pitchers', fname_pitcher))
  pitcher$ab_id = factor(pitcher$ab_id)
  
  pitcher_net <- pitcher %>% 
    mutate(pitch_type = case_when(pitch_num == 1 ~ 
                                    paste(as.character(pitch_num), 
                                          as.character(pitch_type), 
                                          sep = "st pitch "), 
                                  pitch_num == 2 ~
                                    paste(as.character(pitch_num), 
                                          as.character(pitch_type), 
                                          sep = "nd pitch "),
                                  pitch_num == 3 ~ 
                                    paste(as.character(pitch_num), 
                                          as.character(pitch_type), 
                                          sep = "rd pitch "),
                                  pitch_num != 1 & pitch_num != 2 & pitch_num != 3 ~
                                    paste(as.character(pitch_num), 
                                          as.character(pitch_type), 
                                          sep = "th pitch "))) %>% 
    dplyr::select(ab_id, pitch_type)
  
  # labeling carts that don't have milk in them to study these
  pitcher_slottag <- pitcher %>% 
    group_by(ab_id, slot) %>% 
    summarise(pitch_type = "", .groups = 'drop') %>% 
    mutate(pitch_type = as.character(slot)) %>% 
    dplyr::select(ab_id, pitch_type)
 
   pitcher_net <-  rbind(pitcher_net, pitcher_slottag) %>% 
    arrange(ab_id)
  
  # regular
  pitcher_list = split(x=pitcher_net$pitch_type, f=pitcher_net$ab_id)
  pitcher_trans = as(pitcher_list, "transactions")
  pitcher_rules = apriori(pitcher_trans, 
                      parameter=list(support=.01, confidence=.5, maxlen=5))
  inspect(pitcher_rules)
  
  sub_pitchrules = subset(pitcher_rules)
  saveAsGraph(sub_pitchrules, file = file.path(path, 'output', 'models', 'pitcher_rules.graphml'))
  
  plot(pitcher_rules)
  plot(pitcher_rules, measure = c("support", "lift"), shading = "confidence")
  plot(pitcher_rules, method='two-key plot')
  plot(head(pitcher_rules, 100, by='lift'), method='graph')
  
}
