
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
      helpText("Select parameters and analyse the performance of different influence analysis metrics 
               in ordering groundwater monitoring wells by influence compared to well-based cross-validation.
               Lower standardised difference score means better performance. 
               The tables show the respective well influence orders."),
      
      selectInput("plume", label = "Plume complexity", choices = plumes),
      selectInput("design", label = "Network design", choices = designs),
      selectInput("wells", label = "Number of wells", choices = wellnrs),
      selectInput("error", label = "Error type", choices = errors),
      sliderInput("snr", label = "Signal to noise ratio", min = 0, max = 0.5, value = 0.15),
      sliderInput("nseg1", label = "nseg 1", min = 1, max = 20, value = 6),
      sliderInput("nseg2", label = "nseg 2", min = 1, max = 20, value = 6),
      sliderInput("nseg3", label = "nseg 3", min = 1, max = 20, value = 6),
      sliderInput("bdeg", label = "bdeg", min = 2, max = 5, value = 2),
      actionButton("action", "Analyse", class = "btn-lg btn-success")
      
    ),
    
    mainPanel(
      h2("Standardised Difference Scores of IA metrics"),
      plotOutput("barchart"),
      
      fluidRow(
        column( width = 3,
                h2("WBCV"),
                tableOutput("table1")
        ),
        column( width = 3,
                h2("CD"),
                tableOutput("table2")
        ),
        column( width = 3,
                h2("COVRATIO"),
                tableOutput("table7")
        ),
        column( width = 3,
                h2("DFFITS"),
                tableOutput("table5")
        ),
        column( width = 3,
                h2("HP"),
                tableOutput("table6")
        ),
        column( width = 3,
                h2("leverage"),
                tableOutput("table3")
        ),
        column( width = 3,
                h2("MADsr"),
                tableOutput("table4")
        )
      )
              )
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
      
      output$barchart <- renderPlot({
        ggplot(data$scores, aes(x=method, y=diff_score)) + 
          ylab("Standardised Difference Score (d/maxd)") +
          xlab("IA Metric") +
          geom_bar(stat='identity') +
          geom_text(aes(label = round(diff_score, digits = 2)), vjust=-.5) +
          ggtitle(paste0("Scenario: ", plume, " ", design, " ", wells, " ", error, "."))
      })
        
      output$table1 <- renderTable({
        data$wbcv_order
      })
      
      output$table2 <- renderTable({
        data$cd_order
      })
      
      output$table7 <- renderTable({
        data$covratio_order
      })
      
      output$table5 <- renderTable({
        data$dffits_order
      })
      
      output$table6 <- renderTable({
        data$hp_order
      })
      
      output$table3 <- renderTable({
        data$leverage_order
      })
      
      output$table4 <- renderTable({
        data$standres_order
      })
      
    })
  
  }, ignoreNULL = F, ignoreInit = T, once = F)
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
