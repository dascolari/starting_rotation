#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)

# 
# Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)




# Define UI for dataset viewer app ----
# ui <- fluidPage(
#   
#   # App title ----
#   titlePanel("Helping the Astros Cheat Legally"),
#   
#   # Sidebar layout with a input and output definitions ----
#   sidebarLayout(
#     
#     # Sidebar panel for inputs ----
#     sidebarPanel(
#       
#       # Input: Selector for choosing dataset ----
#       selectInput(inputId = "variable",
#                   label = "Variable",
#                   choices = c("Clayton Kershaw", "Yu Darvish", "Aroldis Chapman")),
#       
#       # Input: Numeric entry for number of obs to view ----
#       numericInput(inputId = "obs",
#                    label = "Number of observations to view:",
#                    value = 10),
#       
#       sliderInput(inputId = "obs",
#                   label = "Number of Outs",
#                   min = 0,
#                   max = 2,
#                   value = 0)
#     ),
#     
#     # Main panel for displaying outputs ----
#     mainPanel(
#       
#       # # Output: Verbatim text for data summary ----
#       # verbatimTextOutput("summary"),
#       # 
#       # # Output: HTML table with requested number of observations ----
#       # tableOutput("view")
#       
#     )
#   )
# )


# Define server logic to summarize and view selected dataset ----
# server <- function(input, output) {
#   
#   # Return the requested dataset ----
#   datasetInput <- reactive({
#     switch(input$dataset,
#            "rock" = rock,
#            "pressure" = pressure,
#            "cars" = cars)
#   })
#   
#   # Generate a summary of the dataset ----
#   output$summary <- renderPrint({
#     dataset <- datasetInput()
#     summary(dataset)
#   })
#   
#   # Show the first "n" observations ----
#   output$view <- renderTable({
#     head(datasetInput(), n = input$obs)
#   })
#   
# }


pitch_ab_2015 <- read.csv("~/School/University of Texas-Austin/Classes/Data Mining/pitch_ab_2015.csv")


ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Fuck da Astros"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    # Input: Selector for variable to plot against mpg ----
    selectInput("pitch_name", "Pitcher:", 
                c("Clayton Kershaw" = 506433,
                  "Yu Darvish" = 519455,
                  "Aroldis Chapman" = 518516,
                  "Jake Arrieta" = 453562)),
    
    # Input: Checkbox for whether outliers should be included ----
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
    # Output: Verbatim text for data summary ----
    verbatimTextOutput("general_summary"),
    
    verbatimTextOutput("situation_summary"),
 
    # Output: HTML table with requested number of observations ----
    tableOutput("view"),
    
    plotOutput(outputId = "pitch_plot", height = "300px")
    
  )
)

server <- function(input, output) {
  
  # Subset data
  selected_pitcher <- reactive({
    req(input$pitch_name)
    pitch_ab_2015 %>%
      filter(
        pitcher_id == input$pitch_name
        )
    
  })
  
  situation <- reactive({
    if (input$no_s_count == FALSE){
      strikes = pitch_ab_2015 %>%
        filter(s_count == input$s_count, pitcher_id == input$pitch_name)
    }
    else {
      strikes = pitch_ab_2015 %>%
        filter(pitcher_id == input$pitch_name)
    }
    if (input$no_b_count == FALSE) {
      balls = strikes %>%
        filter(b_count == input$b_count)
    }
    else {
      print("No specified situation")
    }
  })
  
  # Generate a summary of the dataset ----
   output$general_summary <- renderPrint({
     pitch_name <- selected_pitcher()
     summary(pitch_name)
   })
  
  output$situation_summary <- renderPrint({
    sit_sum <- situation()
    summary(sit_sum)
  })
  
  output$pitch_plot <- renderPlot({
    test <- selected_pitcher()
    test$pitch_type = as.factor(test$pitch_type)
    ggplot(test)+
      geom_point(aes(x=spin_dir,y=spin_rate,color=pitch_type),alpha=.2)
    
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)
