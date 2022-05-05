library(tidyverse)
library(shiny)
library(kableExtra)

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

pitch_ab_2015 <- read.csv("~/School/University of Texas-Austin/Classes/Data Mining/pitch_ab_2015.csv")
player_names <- read.csv("~/School/University of Texas-Austin/Classes/Data Mining/starting_rotation/starting_rotation/data/raw/archive/archive/player_names.csv")
load("C:/Users/Student/Documents/School/University of Texas-Austin/Classes/Data Mining/starting_rotation/starting_rotation/output/pitches_import.RData")

physics = pitch_ab_2015 %>%
  dplyr::select(pitch_type,px,pz,start_speed,end_speed,spin_rate,spin_dir,break_angle,break_length,break_y)

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


ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Fuck da Astros"),
  
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
    
    tableOutput("pitch_sum"),
    
    plotOutput(outputId = "spin_plot", height = "300px"),
    
    plotOutput("break_plot", height = "300px"),
    
    verbatimTextOutput("situation_summary")
    
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
  
  situation <- reactive({
    sit_data = selected_pitcher()
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
  })
  
  output$pitch_sum <- renderTable({
    pitch_sum <- selected_pitcher()
    pitch_sum = pitch_sum %>%
      dplyr::select(pitch_type,px,pz,start_speed,end_speed,spin_rate,spin_dir,break_angle,break_length,break_y)%>%
      group_by(pitch_type)%>%
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  })
  
  
   output$general_summary <- renderPrint({
     pitch_name <- selected_pitcher()
     summary(pitch_name)
   })
  
  output$situation_summary <- renderPrint({
    sit_sum <- situation()
    summary(sit_sum)
  })
  
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
