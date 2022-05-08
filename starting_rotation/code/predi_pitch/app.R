library(here)
here::i_am("code/dugout.R")
path <- here()

source(file.path(path, 'code', 'dugout.R'))

load(file = file.path(path, 'output', 'tables', "overall_performance_all.RDs"))

id = 1

foreach(id = 1:30) %do% {
  # make filenames according to loop index
  fname_type <- paste0("types", id, ".RDs")
  fname_zones <- paste0("zones", id, ".RDs")
  fname_bypitch <- paste0("by_pitch_performance", id, ".RDs")
  
  # load table outputs according to loop index
  load(file = file.path(path, 'output', 'tables', fname_type))
  load(file = file.path(path, 'output', 'tables', fname_zones))
  load(file = file.path(path, 'output', 'tables', fname_bypitch))
}

player_names <- read.csv(file.path(path, "data","raw","archive","archive","player_names.csv"))
load(file.path(path, "output", "pitches_import.RData"))

name_list = c(453562,506433,519455,518516,519242,446372,456034,
              544931,453286,433587,477132,594798,502042,425844,
              453343,572096,547973,502202,434718,572971,474521,
              502154,543521,543243,451584,453192,502381,543037,
              608379,518452)

starters = filter(player_names, id %in% name_list)

starters = starters %>%
  mutate(name = paste(first_name,last_name,sep=" "))%>%
  rownames_to_column('new_id_index')

test_list = list(572096,433587,502042,502154,451584,572971,446372,
                 502381,456034,453286,547973,477132,518516,543243,
                 434718,502202,425844,474521,519455,594798,543037,
                 453343,453562,453192,544931,543521,608379,519242,
                 518452,506433)

names(test_list) = c(starters[,4])


ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Pitch Prediction"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    selectInput("pitch_name", "Pitcher:", test_list),
    
    selectInput("stand", "Batter's Stance:",
                c("Left-handed" = "L", "Right-handed" = "R")),
    
    checkboxInput("no_stance", "Check if Batter's Stance is unknown/irrelevant", TRUE),
    
    sliderInput("o", "Number of Outs:",
                min = 0,
                max = 2, 
                value = 0),
    
    checkboxInput("no_o", "Check if Outs are unknown/irrelevant", TRUE),
    
    sliderInput("b_count", "Number of Balls:",
                min = 0,
                max = 3, 
                value = 0),
    
    checkboxInput("no_b_count", "Check if number of balls is unknown/irrelevant", TRUE),
    
    sliderInput("s_count", "Number of Strikes:",
                min = 0,
                max = 2, 
                value = 0),
    
    checkboxInput("no_s_count", "Check if number of strikes is unknown/irrelevant", TRUE)
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    tableOutput("situation"),
    
    tableOutput("pitch_sum"),
    
    plotOutput(outputId = "spin_plot", height = "300px"),
    
    plotOutput("break_plot", height = "300px")
    
  )
)

server <- function(input, output) {
  
  # Subset data
  selected_pitcher <- reactive({
    req(input$pitch_name)
    pitches %>%
      filter(pitcher_id == input$pitch_name)%>%
      mutate(pitch_type = as.factor(pitch_type))%>%
      filter(pitch_type != "")%>%
      filter(pitch_type != "IN")
    
  })
  
  output$situation <- renderTable({
    # Retrieve 1-30 id
    choice = starters %>%
      filter(id == input$pitch_name)
    
    id = as.numeric(choice[1,1])
    
    fname_model <- paste0("trashcans", id, ".RData")
    load(file = file.path(path, 'output', 'models', fname_model))
    
    fname_pitcher <- paste0("pitcher", id, ".Rds")
    load(file = file.path(path, 'output', 'pitchers', fname_pitcher))
    
    fname_factor <- paste0("pitcher_factor", id, ".RDs")
    load(file = file.path(path, 'output', 'pitchers', fname_factor))
    
    pitcher_first <- choice$first_name[1]
    pitcher_last <- names$last_name[1]
    
    pitcher$pitch_type <- factor(pitcher$pitch_type, factor_types)
    pitcher_predi <- pitcher %>% 
      mutate(pitchhat_trashcan = predict(trashcan_3, pitcher))
    pitcher_predi$pitchhat_trashcan = factor(pitcher_predi$pitchhat_trashcan, factor_types)
    
    sit_data = pitcher_predi
    if (input$no_s_count == FALSE || input$no_b_count == FALSE || input$no_o == FALSE || input$no_stance == FALSE){
      if (input$no_s_count == FALSE){
        sit_data = sit_data %>%
          filter(s_count == input$s_count)
      }
      else {
        sit_data
      }
      if (input$no_b_count == FALSE) {
        sit_data = sit_data %>%
          filter(b_count == input$b_count)
      }
      else {
        sit_data
      }
      if (input$no_o == FALSE){
        sit_data = sit_data %>%
          filter(outs == input$o)
      }
      else {
        sit_data
      }
      if(input$no_stance == FALSE){
        sit_data = sit_data %>%
          filter(stand == input$stand)
      }
      else {
        sit_data
      }
    }
    else {
      print("No specified situation")
    }
    
    pitcher_guess <- sit_data %>% 
      group_by(pitchhat_trashcan) %>% 
      summarize(prediction = n()) %>% 
      mutate(percent = prediction / sum(prediction)) %>% 
      rename(pitch_type = pitchhat_trashcan)
    
    pitcher_real <-  sit_data %>% 
      group_by(pitch_type) %>% 
      summarize(actual = n()) %>% 
      mutate(actual_percent = actual / sum(actual))
    
    pitcher_result <- sit_data %>% 
      mutate(success_trashcan = ifelse(pitch_type == pitchhat_trashcan, 1, 0))
    
    situation <- merge(pitcher_guess, pitcher_real)
    situation = situation %>%
      arrange(desc(percent))
    situation
  })
  
  output$pitch_sum <- renderTable({
    pitch_sum <- selected_pitcher()
    pitch_sum = pitch_sum %>%
      dplyr::select(pitch_type,px,pz,start_speed,end_speed,spin_rate,spin_dir,break_angle,break_length,break_y)%>%
      group_by(pitch_type)%>%
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  })
  
  #output$situation_summary <- renderTable({
  #  situation
  #})
  
  output$spin_plot <- renderPlot({
    spins <- selected_pitcher()
    ggplot(spins)+
      geom_point(aes(x=spin_dir,y=spin_rate,color=pitch_type),alpha=.2)
  })
  
  output$break_plot <- renderPlot({
    breaking <- selected_pitcher()
    ggplot(breaking)+
      geom_point(aes(x=break_angle,y=break_length,color=pitch_type),alpha=.2)
  })
  
}

shinyApp(ui = ui, server = server)
