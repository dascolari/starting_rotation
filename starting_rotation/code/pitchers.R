load(file.path(path, "data", "pitches.RDs"))

foreach(id = 1:30, .combine = rbind) %do% {

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
                  inning, 
                  slot)
  pitcher_first <- names$first_name[id]
  pitcher_last <- names$last_name[id]
  
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
  
  tps <- paste("types", pitcher_first, pitcher_last, sep = "_")
  zns <- paste("zones", pitcher_first, pitcher_last, sep = "_")
  assign(tps, table_types)
  assign(zns, table_zones)
  fname_type <- paste0("types", id, ".RDs")
  fname_zones <- paste0("zones", id, ".RDs")
  save(file = file.path(path, 'output', 'tables', fname_type), list = tps)
  save(file = file.path(path, 'output', 'tables', fname_zones), list = zns)
  
  # preserve order of most used as a factor
  # set the factor levels of zone equal to this order
  # will use this order for pitch type tables later on
  factor_types <- factor(types$pitch_type, types$pitch_type)
  factor_zones <- factor(zones$zone, zones$zone)
  pitcher$zone <- factor(pitcher$zone, factor_zones)
  
  fname_factor <- paste0("pitcher_factor", id, ".RDs")
  save(file = file.path(path, 'output', 'pitchers', fname_factor), list = "factor_types")
  
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
  
  inning_stuff <- pitches %>% 
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
    mutate_at(vars(- c("g_id", "total")), ~./total)
  
  max_inning <- max(pitcher$inning)
  
  gameday_stuff <- 
    foreach(i = 1:max_inning, .combine = rbind) %do% {
    inning_stuff %>% mutate(inning = i)
  }%>% mutate_at(vars(- c("g_id", "total", "inning")), ~ifelse(inning <= 4, 0, .))
  
  pitcher <- merge(pitcher, gameday_stuff, by = c("g_id", "inning"), all.x = TRUE) %>% 
    replace(is.na(.), 0)
  
  # ## make quick changes by manually setting id
  # id = 
  
  fname_pitcher <- paste("pitcher", id, ".RDs", sep = "")
  save(file = file.path(path, 'output', 'pitchers', fname_pitcher), list = "pitcher")
}
