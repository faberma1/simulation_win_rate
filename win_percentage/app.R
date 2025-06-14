library(tidyverse)
library(gt)
library(shiny)
library(ggtext)
library(rsconnect)

simulator <- function(n1, n2, p) {
  results <- integer(10000)
  
  for (i in 1:10000) {
    wr1 <- mean(runif(n1) < p)
    wr2 <- mean(runif(n2) < p)
    
    results[i] <- ifelse(wr1 > wr2, 1, ifelse(wr1 < wr2, -1, 0))
  }
  
  results_table <- tibble(
    n1_win = mean(results == 1),
    n2_win = mean(results == -1),
    tie    = mean(results == 0)
  )
  
  return(results_table)
}


ui <- fluidPage(

    
    titlePanel("Success Rate Simulator"),


      sidebarLayout(
        sidebarPanel(
          sliderInput("success_rate", "Probability of Success:",
                      min = 0, max = 1, value = 0.1, step = 0.01, ticks = FALSE),
          sliderInput("n1_slider", "Trials (1):",
                      min = 1, max = 10000, value = 10, step = 1, ticks = FALSE),
          
          numericInput("n1_numeric", "Or enter exact Trials (1):",
                       value = 10, min = 1, max = 10000, step = 1),
          sliderInput("n2_slider", "Trials (2):",
                      min = 1, max = 10000, value = 100, step = 1, ticks = FALSE),
          numericInput("n2_numeric", "Or enter exact Trials (2):",
                       value = 100, min = 1, max = 10000, step = 1),
          actionButton("simulate_btn", "Simulate", style = "color: white; background-color: #0072B2; border-color: #005b96;"),
        ),

    
        mainPanel(
          h1("Results", style = "text-align: center;"),
          div(style = "width: 100%;",
              plotOutput("bar_graph")),
        )
        
    ),
    absolutePanel(
      top = 10, right = 10,
      width = "auto", height = "auto",
      style = "background-color: transparent; font-size: 12px; color: gray;",
      "Created by Shane Faberman"
    )
    )

server <- function(input, output, session) {
  
  observeEvent(input$n1_slider, {
    updateNumericInput(session, "n1_numeric", value = input$n1_slider)
  })
  
  observeEvent(input$n1_numeric, {
    updateSliderInput(session, "n1_slider", value = input$n1_numeric)
  })
  
  observeEvent(input$n2_slider, {
    updateNumericInput(session, "n2_numeric", value = input$n2_slider)
  })
  
  observeEvent(input$n2_numeric, {
    updateSliderInput(session, "n2_slider", value = input$n2_numeric)
  })
  
  reactive_sim <- eventReactive(input$simulate_btn, {
    withProgress(message = "Simulating...", value = 0.2, {
      simulator(input$n1_slider, input$n2_slider, input$success_rate)
    })
  })
  
  
  output$bar_graph <- renderPlot({
    df <- reactive_sim()
    
    df %>% 
      rename(`1` = n1_win, `2`= n2_win, Tie = tie) %>% 
      pivot_longer(cols = 1:3,names_to = "event", values_to = "value") %>% 
      mutate(color = ifelse(value == max(value), "lightblue", ifelse(value == min(value), "gray", "#FFADB0"))) %>% 
      ggplot(aes(x = event, y = value, fill = color)) +
      geom_bar(stat = 'identity')+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_identity() +
      scale_x_discrete(labels = c(
        "1" = paste0(input$n1_slider, " trials"),
        "2" = paste0(input$n2_slider, " trials"),
        "Tie" = "Tie"
      )) +
      labs(x = "",
           y = "",
           title = "Which trial count had the <span style='color:lightblue'>higher</span> or <span style='color:#FFADB0'>lower</span> success rate more frequently?") +
      geom_text(aes(label = scales::percent(value, accuracy = 0.1)), 
                vjust = -0.5, size = 4)+
      theme_minimal() +
      theme(plot.title = element_markdown(face = "bold", hjust = 0.5, size = 16),
            text = element_text(family = "Times New Roman"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            axis.text = element_text(color = "black", size = 14))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

