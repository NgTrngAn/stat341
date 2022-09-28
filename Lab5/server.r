source("plot_generators.r")
library(shiny)

server <- function(input, output) {
    observeEvent(
        input$plotHyperGeom,
        output$HyperGeom <- renderPlot(plot.hyper(input$mHyper, 
                                input$NHyper - input$mHyper,
                                input$drawHyper))
    )

    observeEvent(
        input$plotNegHyperGeom,
        output$NegHyperGeom <- renderPlot(plot.nhyper(input$mNegHyper, 
                                input$NNegHyper - input$mNegHyper,
                                input$rNegHyper))
    )

    observeEvent(
        input$plotPoisson,
        output$Poisson <- renderPlot(plot.pois(input$rate))
    )
}
