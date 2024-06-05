library(shiny)
library(gridExtra)
library(ggpubr)
library(ggplot2)



ui <- fluidPage(
  titlePanel(
    HTML("Estimated 'Profit Improvement Potential' Calculator <br> (your actual results will vary from this estimate)")),
  
  fluidRow(
    column(3,
           textInput("heading",
                     "Begin Here:",
                     "Enter Your Current Figures Below"),
           
           numericInput("customers",
                        "Number of Customers",
                        value = 300,
                        min = 0,
                        step = 10),
           
           numericInput("transactions",
                        "Transactions per Year",
                        value = 1,
                        min = 0,
                        step = 1),
           
           numericInput("value",
                        "Average Transaction Value",
                        value = 1000,
                        min = 0,
                        step = 1),
           
           numericInput("profit",
                        "Net Profit Percentage (before fixed expenses)",
                        value = 1.0,
                        min = 0,
                        max = 1,
                        step = 0.01)
    ),
    column(3,
           textInput("heading_change",
                     "Enter Your Estimated Improvement % Below:",
                     "% Changes"),
           
           numericInput("customers_change",
                        "Customers % Change (enter as decimal)",
                        value = 0,
                        min = -1,
                        max = 1, 
                        step = 0.01),
           
           numericInput("transactions_change",
                        "Transactions % Change (enter as decimal)",
                        value = 0,
                        min = -1,
                        max = 1,
                        step = 0.01),
           
           numericInput("value_change",
                        "Transaction Value % Change (enter as decimal)",
                        value = 0,
                        min = -1,
                        max = 1,
                        step = 0.01),
           
           numericInput("profit_change",
                        "Profit % Change (enter as decimal)",
                        value = 0,
                        min = -1,
                        max = 1,
                        step = 0.01)
    ),
    column(3,
           actionButton("submit",
                        "Calculate"),
           downloadButton('downloadPDF',
                          'Download PDF')
    )
  ),
  
  mainPanel(
    tableOutput("current_state"),
    tableOutput("improved_state"),
    textOutput("improvement"),
    verbatimTextOutput("calculation")
  )
)

#################################

server <- function(input, output) {
  profit_before <- reactive({
    req(input$submit)
    isolate({
      input$customers * input$transactions * input$value * input$profit
    })
  })
  
  improved_state <- reactive({
    req(input$submit)
    isolate({
      revised_customers <- input$customers * (1 + input$customers_change)
      revised_transactions <- input$transactions * (1 + input$transactions_change)
      revised_value <- input$value * (1 + input$value_change)
      revised_profit <- input$profit * (1 + input$profit_change)
      revised_customers * revised_transactions * revised_value * revised_profit
    })
  })
  
  output$improvement <- renderText({
    req(input$submit)
    isolate({
      formatC(improved_state() - profit_before(),
              format = "f",
              big.mark = ",",
              digits = 0)
    })
  })
  
  output$calculation <- renderPrint({
    req(input$submit)
    isolate({
      revised_customers <- input$customers * (1 + input$customers_change)
      revised_transactions <- input$transactions * (1 + input$transactions_change)
      revised_value <- input$value * (1 + input$value_change)
      revised_profit <- input$profit * (1 + input$profit_change)
      cat("Revised Customers: ",
          revised_customers,
          "\n",
          "Revised Transactions: ",
          revised_transactions,
          "\n",
          "Revised Transaction Value: $",
          formatC(revised_value,
                  format = "f",
                  big.mark = ",",
                  digits = 0),
          "\n",
          "Revised Profit Percentage: ",
          formatC(revised_profit * 100,
                  format = "f",
                  big.mark = ",",
                  digits = 2),
          "%\n",
          "Profit in Possible Improved State: $",
          formatC(improved_state(),
                  format = "f",
                  big.mark = ",",
                  digits = 0),
          "\n",
          "Profit in Current State: $", 
          formatC(profit_before(),
                  format = "f",
                  big.mark = ",",
                  digits = 0), 
          "\n",
          "Estimated Profit Improvement Potential: $",
          formatC(improved_state() - profit_before(),
                  format = "f",
                  big.mark = ",",
                  digits = 0))
    })
  })
  
  ########################################
  
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste("calculation-details", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Create a data frame with the desired content
      calculation_data <- data.frame(
        Description = c("Possible Profit in Improved State:",
                        "Profit in Current State:",
                        "Estimated Profit Improvement Potential:"
                        ),
        Value = c(
          paste("$", formatC(improved_state(), format = "f", big.mark = ",", digits = 0)),
          paste("$", formatC(profit_before(), format = "f", big.mark = ",", digits = 0)),
          paste("$", formatC(improved_state() - profit_before(), format = "f", big.mark = ",", digits = 0))
          )
      )
      
      # Create a ggtexttable
      table <- ggtexttable(calculation_data,
                           rows = NULL,  # Omit row numbers
                           theme = ttheme('mBlue', base_size = 12),
                           )
      
      table <- tab_add_title(table,
                    "Results")
      
      # Save the ggtexttable to a PDF file
      pdf(file, onefile = TRUE, width = 15, height = 9)
      grid.draw(table)

      ## PDF REPORT DISCLAIMER
      grid.text(paste("(YOUR ACTUAL RESULTS WILL VARY FROM THIS ESTIMATE) \n Calculator created on 2023-07-09 by Dan Swart.  Report date: ", Sys.Date()), x = 0.5, y = 0.1, gp = gpar(fontsize = 12))
      
      dev.off()
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)
