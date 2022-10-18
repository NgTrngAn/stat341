source("plot_generators.r")
library(shiny)
library(EnvStats)

server <- function(input, output) {
    observeEvent(
        input$plotUnif,
        output$Unif <- renderPlot(plot.unif(input$lowerUnif,
                                            input$upperUnif))
    )

    observeEvent(
        input$plotNormal,
        output$Normal <- renderPlot(plot.norm(input$muNormal,
                                              input$sigmaNormal))
    )

    observeEvent(
        input$plotGamma,
        output$Gamma <- renderPlot(plot.gamma(input$shapeGamma,
                                              input$scaleGamma))
    )

    observeEvent(
        input$plotExp,
        output$Exp <- renderPlot(plot.exp(input$rateExp))
    )

    observeEvent(
        input$plotChisq,
        output$Chisq <- renderPlot(plot.chisq(input$dfChisq))
    )

    observeEvent(
        input$plotBeta,
        output$Beta <- renderPlot(plot.beta(input$shapeBeta,
                                              input$scaleBeta))
    )

    # activity 2
    output$randuSummary <- renderPrint(
        {summary(randu)}
    )

    data("randu")

    output$randuHistogram <- renderPlot(
        {
            randuVar <- switch(input$randuVarChoice,
                               randux = randu$x,
                               randuy = randu$y,
                               randuz = randu$z
            )

            hist(randuVar, 
                 main="Histogram of the selected variable from randu",
                 xlab="value")
        }
    )


    observeEvent(
        input$plotqq,
        output$qqplot <- renderPlot(
            {
                x <- switch(
                    input$randuEmp,
                    randuempx = randu$x,
                    randuempy = randu$y,
                    randuempz = randu$z
                )

                # setting parameters to go with the distribution
                if (input$theoreticalDist == 'beta') {
                    parameters <- list(shape1 = input$qqShapeBeta, shape2 = input$qqScaleBeta)
                } else if (input$theoreticalDist == 'norm') {
                    parameters <- list(mean = input$qqMuNormal, sd = input$qqSigmaNormal)
                } else if (input$theoreticalDist == 'chisq') {
                    parameters <- list(df = input$qqDfChisq)
                } else if (input$theoreticalDist == 'exp') {
                    parameters <- list(rate = input$qqRateExp)
                } else if (input$theoreticalDist == 'gamma') {
                    parameters <- list(shape = input$qqShapeGamma, scale = input$qqScaleGamma)
                } else if (input$theoreticalDist == 'unif') {
                    parameters <- list(min = input$qqLowerUnif, max = input$qqUpperUnif)
                } else {
                    parameters <- NULL
                }

                qqPlot(x, distribution = input$theoreticalDist, param.list = parameters)
            }
        )
    )

}
