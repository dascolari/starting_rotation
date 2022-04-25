library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("More Widgets"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a dataset ----
      selectInput("stance", "Choose a dataset:",
                  choices = c("R", "L")),
      
      # Input: Specify the number of observations to view ----
      numericInput("outs", "Number of outs:", 0),
      numericInput("strikes", "Number of strikes:", 0),
      numericInput("balls", "Number of balls:", 0),
      
      # Include clarifying text ----
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton("update", "Update View")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Header + table of distribution ----
      h4("Pitcher Predictions"),
      tableOutput("view")
    )
    
  )
)

server <- function(input, output) {
  
  load(file.path(path, "output", "pitches_import.RData"))
  
  id = 12
  stance <- reactive({
    input$stance
  }) 
  strikes <- reactive({
    input$strikes
  })
  balls <- reactive({
    input$balls
  })
  outs <- reactive({
    input$outs
  })
  
  
  fname_model <- paste0("trashcan", id, ".RDs")
  load(file = file.path(path, 'output', 'models', fname_model))
  
  fname_pitcher <- paste0("pitcher", id, ".Rds")
  load(file = file.path(path, 'output', 'pitchers', fname_pitcher))
  
  fname_factor <- paste0("pitcher_factor", id, ".RDs")
  load(file = file.path(path, 'output', 'pitchers', fname_factor))
  
  pitcher_first <- names$first_name[id]
  pitcher_last <- names$last_name[id]
  
  # out of sample performance (osp)
  # re-factor to get most thrown pitch first in each table
  pitcher$pitch_type <- factor(pitcher$pitch_type, factor_types)
  pitcher_predi <- pitcher %>% 
    mutate(pitchhat_trashcan = predict(trashcan, pitcher))
  pitcher_predi$pitchhat_trashcan = factor(pitcher_predi$pitchhat_trashcan, factor_types)
  
  pitcher_result <- pitcher_predi %>% 
    filter(stand == stance, s_count == strikes, b_count == balls, outs == outs)
  
  pitcher_guess <- pitcher_result %>% 
    group_by(pitchhat_trashcan) %>% 
    summarize(prediction = n()) %>% 
    mutate(percent = prediction / sum(prediction)) %>% 
    rename(pitch_type = pitchhat_trashcan)
  
  pitcher_real <-  pitcher_result %>% 
    group_by(pitch_type) %>% 
    summarize(actual = n()) %>% 
    mutate(actual_percent = actual / sum(actual))
  
  pitcher_result <- pitcher_result %>% 
    mutate(success_trashcan = ifelse(pitch_type == pitchhat_trashcan, 1, 0)) 
  
  #  pitcher_confidence <-  pitcher_result %>% 
  #    group_by(pitch_type) %>% 
  #    summarize(confidence = sum(success_trashcan) / n())
  
  situation <- merge(pitcher_guess, pitcher_real)
  #  situation <- merge(situation, pitcher_confidence)
  
  output$view <- renderTable({
    situation
  })
  
}

shinyApp(ui, server)
