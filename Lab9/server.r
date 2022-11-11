source("plot_generators.r")
library(shiny)
library(extraDistr)

server <- function(input, output) {
    
    getSample <- reactive(sample(c(0:9), input$trialsNum, replace = TRUE))

    observeEvent(
        input$roll,
        output$table1 <- renderTable({
            outcomes <- getSample()
            table <- as.data.frame(rbind(c(1:30), head(outcomes, 30)))
            rownames(table) <- c("Roll", "Value")
            table            
        }, rownames = TRUE, colnames = FALSE 
        ),

        output$plot1 <- renderPlot({
            outcomes <- getSample()
            hist(outcomes)
        })

        
    )

    
}
