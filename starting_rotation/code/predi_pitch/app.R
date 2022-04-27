library(tidyverse)
library(shiny)

pitch_ab_2015 <- read.csv("~/School/University of Texas-Austin/Classes/Data Mining/pitch_ab_2015.csv")
stats <- read.csv("~/School/University of Texas-Austin/Classes/Data Mining/starting_rotation/starting_rotation/data/stats.csv")


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
    
    tableOutput("view"),
    
    tableOutput("pitch_sum"),
    
    plotOutput(outputId = "spin_plot", height = "300px"),
    
    plotOutput("break_plot", height = "300px"),
    
    verbatimTextOutput("general_summary"),
    
    verbatimTextOutput("situation_summary")
    
  )
)

server <- function(input, output) {
  
  # Subset data
  selected_pitcher <- reactive({
    req(input$pitch_name)
    pitch_ab_2015 %>%
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

  output$view <- renderTable({
    career_tab = stats %>%
      filter(player_id == input$pitch_name) %>%
      arrange(year)
  })
  
  output$pitch_sum <- renderTable({
    pitch_sum <- selected_pitcher()
    pitch_sum = pitch_sum %>%
      select(pitch_type,px,pz,start_speed,end_speed,spin_rate,spin_dir,break_angle,break_length,break_y)%>%
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
