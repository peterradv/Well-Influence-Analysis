
library(shiny)
library(tidyverse)

# getwd()
# setwd()
source("sim_study_function.R")

plumes <- c("simple", "mid", "complex")
designs <- c("random", "grid", "expert")
wellnrs <- c(6, 12, 24)
errors <- c("additive", "multiplicative")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Well Influence Analysis Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select parameters and analyse the performance of different metrics 
               in quantifying the influence of individual groundwater monitoring wells.
               The lower the Standardised Difference Score the better."),
      
      selectInput("plume", label = "Plume complexity", choices = plumes),
      selectInput("design", label = "Network design", choices = designs),
      selectInput("wells", label = "Number of wells", choices = wellnrs),
      selectInput("error", label = "Error type", choices = errors),
      sliderInput("snr", label = "Signal to noise ratio", min = 0, max = 0.5, value = 0.15),
      sliderInput("nseg1", label = "nseg 1", min = 1, max = 20, value = 6),
      sliderInput("nseg2", label = "nseg 2", min = 1, max = 20, value = 6),
      sliderInput("nseg3", label = "nseg 3", min = 1, max = 20, value = 6),
      sliderInput("bdeg", label = "bdeg", min = 2, max = 5, value = 2),
      actionButton("action", "Analyze!", class = "btn-lg btn-success")
      
    ),
    
    mainPanel(plotOutput("barchart"))
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$action, {
    
    output$barchart <- renderPlot({
      
      plume <- input$plume

      design <- input$design

      wells <- input$wells

      error <- input$error

      snr <- input$snr

      nseg <- c(input$nseg1, input$nseg2, input$nseg3)

      bdeg <- input$bdeg
      
      data <- well_influence_sim(plume, design, wells, error, snr, nseg, bdeg)
      
      ggplot(data, aes(x=method, y=diff_score)) + 
        ylab("Standardised Difference Score (d/maxd)") +
        xlab("IA Metric") +
        geom_bar(stat='identity') +
        geom_text(aes(label = round(diff_score, digits = 2)), vjust=-.5) +
        ggtitle(paste0("Standardised Difference Scores. Scenario: ", plume, " ", design, " ", wells, " ", error, "."))
    
      })
  
  }, ignoreNULL = F, ignoreInit = T, once = F)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
