library(shiny)
library(Quandl)
library(YieldCurve)
library(DT)
library(ggplot2)
library(shinythemes)

#API
observe({
  
  invalidateLater(3600000, session = NULL)
  source("utils/api_refresh.R")

  })
  
# Define UI for application that draws a histogram
ui <- fluidPage(
   themeSelector(),
   
   # Application title
   titlePanel("Yield curve"),
   
   # Sidebar with inputs for euribor and search dates
   sidebarLayout(
      sidebarPanel(
          selectInput(inputId = "euribor",
                    label = "Choose the euribor", 
                    choices = c("Euribor 1m" = 1,
                                "Euribor 3m" = 3,
                                "Euribor 6m" = 6, 
                                "Euribor 9m" = 9, 
                                "Euribor 12m" = 12),
                    selected = 1, 
                    multiple = FALSE
                    ), 
          
          br(),
        
          dateInput(inputId = "select_date",
                  label = "Select the date", 
                  weekstart = 1,
                  value = Sys.Date()-2,
                  max = Sys.Date()-2
                  )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tableOutput("spot_euribor_table"),
        
        plotOutput("euribor_plot"),
       
        dataTableOutput("euribor_table")
        
       )
   )
)

# Define server logic

server <- function(input, output) {
  
  # Reactive conductors
  spot_euribor <- reactive({
    req(input$select_date)
    
    svensson_parameters <- Svensson(rate = euribor_db[input$select_date,], 
                                    maturity = c(1,3,6,9,12))
    
    Srates(Coeff = svensson_parameters,
           maturity = seq(1:48),
           whichRate = "Spot")
  })
  
  results <- reactive({
    req(input$select_date)
    req(input$euribor)
    
    results <- data.frame()
    
    for (i in 1:length(spot_euribor())){
      j<- i+as.numeric(input$euribor)
      
      if(j >length(spot_euribor())){
        next()
        
      }else{
        
        results[1,i] <- (((((1+spot_euribor()[,j]/(12*100))^j)/((1+spot_euribor()[,i]/(12*100))^i))^(1/as.numeric(input$euribor)))-1)*12*100
        
      }
      
    }
    
    results<- xts(results, order.by = input$select_date) 
    
  })
  
  #output 1st table
  output$spot_euribor_table <- renderTable(expr = {
    
    table <- spot_euribor()
    colnames(table) <- paste("Spot euribor ", seq(1:48))
    table
    
    },
  bordered = TRUE,digits = 4) 
  
  
  #output 2nd table
  output$euribor_table <- renderDataTable(expr = {
    
    fwrd_svensson_parameters <- Svensson(rate= results(),
                                         maturity = seq(1:length(results())))


    euribor_yc_linear_fwrd <- Srates(Coeff = fwrd_svensson_parameters,
                                     maturity = seq(1:48),
                                     whichRate = "Spot")

    colnames(euribor_yc_linear_fwrd) <- paste("In ",seq(1:48)," months")
    euribor_yc_linear_fwrd <- t(euribor_yc_linear_fwrd)
  })
  
  ##
  
  
  ##Plot
  output$euribor_plot <- renderPlot(expr ={

    fwrd_svensson_parameters <- Svensson(rate= results(),
                                         maturity = seq(1:length(results())))
    
    
    euribor_yc_linear_fwrd <- Srates(Coeff = fwrd_svensson_parameters,
                                     maturity = seq(1:48),
                                     whichRate = "Spot")
    
    plot(x = seq(1:48),y = euribor_yc_linear_fwrd,type = "o")
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)