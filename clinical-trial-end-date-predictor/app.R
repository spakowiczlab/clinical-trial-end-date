# app for predicting the completion date of clinical trials

library(shiny)
library(lubridate)


# Define the UI for an application that predicts the end date of a trial given several inputs
ui <- 
    
    fluidPage(
        
        # Application title
        titlePanel("Clinical Trial End Date Predictor"),
        
        sidebarLayout(
            sidebarPanel(
                dateInput("todays.date", 
                          "Today's Date"),
                numericInput("weeks.open", 
                            "Number of Weeks the trial has been open", 
                            min = 1, max = 1000, value = 1, step = 1),
                numericInput("accrual.target", 
                            "Accrual Target", 
                            min = 1, max = 2000, value = 63, step = 1),
                numericInput("current.accrual", 
                            "Current Accrual", 
                            min = 0, max = 2000, value = 5, step = 1),
                numericInput("study.duration", 
                            "Study Duration in weeks", 
                            min = 1, max = 500, value = 16, step = 1)
            ),
            
            mainPanel(
                textOutput("date.full.accrual")
            )
        )
    )

server <- function(input, output) {
  
    date.full.accrual <- reactive({
        patients.to.go <- input$accrual.target - input$current.accrual
        
        weeks.to.full.accrual <- ceiling(patients.to.go / 
                                             (input$current.accrual / 
                                                  input$weeks.open))
        
        input$todays.date + 
            lubridate::weeks(weeks.to.full.accrual)
    })
    
    date.study.completion <- reactive({
        patients.to.go <- input$accrual.target - input$current.accrual
        
        weeks.to.full.accrual <- ceiling(patients.to.go / 
                                             (input$current.accrual / 
                                                  input$weeks.open))
        
        weeks.to.completion <- weeks.to.full.accrual + input$study.duration
        
        input$todays.date + 
            lubridate::weeks(weeks.to.completion)
    })
    
    output$date.full.accrual <- renderText({
        paste0("Date of full accrual = ", date.full.accrual(), "           ",
               "Date of study completion = ", date.study.completion())
    })
}
    
# Run the application 
shinyApp(ui = ui, server = server)
