library(shiny)
library(ggplot2)
#setwd("~/Box Sync/STAT-341-DMO/Fall2022/Homework")
# setwd("~/Library/CloudStorage/Box-Box/STAT-341-DMO/Fall2022/Homework")
source("PlotFunctions.R")
#runApp("~/Library/CloudStorage/Box-Box/STAT-341-DMO/Fall2022/Homework/ContinuousProb_shiny")

# Define UI for app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("STAT 341: Computing Continuous Probabilities"),
  
  # Output: Tabset for different Activities ----
  tabsetPanel(type = "tabs",
              tabPanel("unif", 
                       h3("Uniform Distribution"),
                       
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: 
                           numericInput(inputId = "Aunif",
                                        label = "A (Minimum)",
                                        value = 0),
                           # Input: 
                           numericInput(inputId = "Bunif",
                                        label = "B (Maximum)",
                                        value = 1),
                           # Input: 
                           radioButtons("fnxUnif", "Probability function:",
                                        c("PMF" = "pmf",
                                          "CDF" = "cdf",
                                          "Quantile" = "quf")),
                           # Input: 
                           uiOutput("valUnif")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: 
                           plotOutput("unifPDF"),
                           # Output: 
                           plotOutput("unifCDF"),
                           # Output: 
                           verbatimTextOutput("unifR")
                         )
                       )),
              tabPanel("norm", 
                       h3("Normal Distributions"),
                       
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: 
                           numericInput(inputId = "muNorm",
                                        label = "mu (Mean)",
                                        value = 0),
                           # Input: 
                           numericInput(inputId = "sigNorm",
                                        label = "sigma (St. Dev.)",
                                        value = 1),
                           # Input: 
                           radioButtons("fnxNorm", "Probability function:",
                                        c("PMF" = "pmf",
                                          "CDF" = "cdf",
                                          "Quantile" = "quf")),
                           # Input: 
                           uiOutput("valNorm")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: 
                           plotOutput("normPDF"),
                           # Output: 
                           plotOutput("normCDF"),
                           # Output:
                           verbatimTextOutput("normR")
                         )
                       )),
              tabPanel("gamma", 
                       h3("Gamma Distributions"),
                       
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: 
                           numericInput(inputId = "alphaGam",
                                        label = "alpha (Shape)",
                                        value = 1,
                                        min = 0),
                           # Input: 
                           numericInput(inputId = "betaGam",
                                        label = "beta (Scale)",
                                        value = 1,
                                        min = 0),
                           # Input: 
                           radioButtons("fnxGamma", "Probability function:",
                                        c("PMF" = "pmf",
                                          "CDF" = "cdf",
                                          "Quantile" = "quf")),
                           # Input: 
                           uiOutput("valGamma")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: 
                           plotOutput("gammaPDF"),
                           # Output: 
                           plotOutput("gammaCDF"),
                           # Output: 
                           verbatimTextOutput("gammaR")
                         )
                       )),
              tabPanel("exp", 
                       h3("Exponential Distributions"),
                       
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: 
                           numericInput(inputId = "lambda",
                                        label = "lambda (Rate)",
                                        value = 1,
                                        min = 0),
                           # Input: 
                           radioButtons("fnxExp", "Probability function:",
                                        c("PMF" = "pmf",
                                          "CDF" = "cdf",
                                          "Quantile" = "quf")),
                           # Input: 
                           uiOutput("valExp")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: 
                           plotOutput("expPDF"),
                           # Output: 
                           plotOutput("expCDF"),
                           # Output: 
                           verbatimTextOutput("expR")
                         )
                       )),
              tabPanel("chisq",
                       h3("Chi-Squared Distributions"),

                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: 
                           numericInput(inputId = "nuDF",
                                        label = "nu (Degrees of Freedom)",
                                        value = 1,
                                        min = 1),
                           # Input: 
                           radioButtons("fnxChiSq", "Probability function:",
                                        c("PMF" = "pmf",
                                          "CDF" = "cdf",
                                          "Quantile" = "quf")),
                           # Input: 
                           uiOutput("valChiSq")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: 
                           plotOutput("chisqPDF"),
                           # Output: 
                           plotOutput("chisqCDF"),
                           # Output: Verbatim text for R code & results ----
                           verbatimTextOutput("chisqR")
                         )
                       )),
              tabPanel("beta",
                       h3("Beta Distribution"),

                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: 
                           numericInput(inputId = "alphaBeta",
                                        label = "alpha (Shape 1)",
                                        value = 1,
                                        min = 0),
                           # Input: 
                           numericInput(inputId = "betaBeta",
                                        label = "beta (Shape 2)",
                                        value = 1,
                                        min = 0),
                           # Input: 
                           radioButtons("fnxBeta", "Probability function:",
                                        c("PMF" = "pmf",
                                          "CDF" = "cdf",
                                          "Quantile" = "quf")),
                           # Input: 
                           uiOutput("valBeta")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: 
                           plotOutput("betaPDF"),
                           # Output: 
                           plotOutput("betaCDF"),
                           # Output: 
                           verbatimTextOutput("betaR")
                         )
                       ))
  )
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  ###################################################################
  # UNIFORM
  
  fUnif <- function(){
    switch(input$fnxUnif, pmf = 1, cdf = 2, quf = 3)
  }
  
  output$valUnif <- renderUI({
    
    if(fUnif() == 1){
      # Input: 
      numericInput(inputId = "valUnif",
                   label = "Value (y)",
                   value = (input$Aunif + input$Bunif)/2,
                   min = input$Aunif, max = input$Bunif)
    }else if(fUnif() == 2){
      # Input: 
      numericInput(inputId = "valUnif",
                   label = "Value (y)",
                   value = (input$Aunif + input$Bunif)/2,
                   min = input$Aunif, max = input$Bunif)
    }else if(fUnif() == 3){
      # Input: 
      numericInput(inputId = "valUnif",
                   label = "Value (p)",
                   value = 0.5,
                   min = 0, max = 1)
    }
  })
  
  output$unifPDF <- renderPlot({
    
    if(fUnif() == 1){
      plot.unif.pdf(as.numeric(input$Aunif), as.numeric(input$Bunif), 
                    fnx=1, val=as.numeric(input$valUnif))
    }else if(fUnif() == 2){
      plot.unif.pdf(as.numeric(input$Aunif), as.numeric(input$Bunif), 
                    fnx=2, val=as.numeric(input$valUnif))
    }else if(fUnif() == 3){
      plot.unif.pdf(as.numeric(input$Aunif), as.numeric(input$Bunif), 
                    fnx=3, val=as.numeric(input$valUnif))
    }
  })
  
  output$unifCDF <- renderPlot({
    
    if(fUnif() == 1){
      plot.unif.CDF(as.numeric(input$Aunif), as.numeric(input$Bunif), 
                    fnx=1, val=as.numeric(input$valUnif))
    }else if(fUnif() == 2){
      plot.unif.CDF(as.numeric(input$Aunif), as.numeric(input$Bunif), 
                    fnx=2, val=as.numeric(input$valUnif))
    }else if(fUnif() == 3){
      plot.unif.CDF(as.numeric(input$Aunif), as.numeric(input$Bunif), 
                    fnx=3, val=as.numeric(input$valUnif))
    }
  })
  
  # Display the R code and resulting probability ----
  output$unifR <- renderPrint({
    if(fUnif() == 1){
      cat(paste("R Code:\n", "dunif(", input$valUnif, ",", input$Aunif, ",", input$Bunif, ")",  
                "\n\nResult:\nf(y) = ", dunif(as.numeric(input$valUnif), 
                                              as.numeric(input$Aunif), 
                                              as.numeric(input$Bunif)), sep=""))
    }
    if(fUnif() == 2){
      cat(paste("R Code:\n", "punif(", input$valUnif, ",", input$Aunif, ",", input$Bunif, ")",  
                "\n\nResult:\nF(y) = P(Y < y) = ", punif(as.numeric(input$valUnif), 
                                                         as.numeric(input$Aunif), 
                                                         as.numeric(input$Bunif)), sep=""))
    }
    if(fUnif() == 3){
      cat(paste("R Code:\n", "qunif(", input$valUnif, ",", input$Aunif, ",", input$Bunif, ")",  
                "\n\nResult:\n y = Q(p) = ", qunif(as.numeric(input$valUnif), 
                                                   as.numeric(input$Aunif), 
                                                   as.numeric(input$Bunif)), sep=""))
    }
  })
  
  
  ###################################################################
  # NORMAL
  
  fNorm <- function(){
    switch(input$fnxNorm, pmf = 1, cdf = 2, quf = 3)
  }
  
  output$valNorm <- renderUI({
    
    if(fNorm() == 1){
      # Input: 
      numericInput(inputId = "valNorm",
                   label = "Value (y)",
                   value = input$muNorm)
    }else if(fNorm() == 2){
      # Input: 
      numericInput(inputId = "valNorm",
                   label = "Value (y)",
                   value = input$muNorm)
    }else if(fNorm() == 3){
      # Input: 
      numericInput(inputId = "valNorm",
                   label = "Value (p)",
                   value = 0.5,
                   min = 0, max = 1)
    }
  })
  
  output$normPDF <- renderPlot({
    
    if(fNorm() == 1){
      plot.norm.pdf(as.numeric(input$muNorm), as.numeric(input$sigNorm), 
                    fnx=1, val=as.numeric(input$valNorm))
    }else if(fNorm() == 2){
      plot.norm.pdf(as.numeric(input$muNorm), as.numeric(input$sigNorm), 
                    fnx=2, val=as.numeric(input$valNorm))
    }else if(fNorm() == 3){
      plot.norm.pdf(as.numeric(input$muNorm), as.numeric(input$sigNorm), 
                    fnx=3, val=as.numeric(input$valNorm))
    }
  })
  
  output$normCDF <- renderPlot({
    
    if(fNorm() == 1){
      plot.norm.CDF(as.numeric(input$muNorm), as.numeric(input$sigNorm), 
                    fnx=1, val=as.numeric(input$valNorm))
    }else if(fNorm() == 2){
      plot.norm.CDF(as.numeric(input$muNorm), as.numeric(input$sigNorm), 
                    fnx=2, val=as.numeric(input$valNorm))
    }else if(fNorm() == 3){
      plot.norm.CDF(as.numeric(input$muNorm), as.numeric(input$sigNorm), 
                    fnx=3, val=as.numeric(input$valNorm))
    }
  })
  
  # Display the R code and resulting probability ----
  output$normR <- renderPrint({
    if(fNorm() == 1){
      cat(paste("R Code:\n", "dnorm(", input$valNorm, ",", input$muNorm, ",", input$sigNorm, ")",  
                "\n\nResult:\nf(y) = ", dnorm(as.numeric(input$valNorm), 
                                              as.numeric(input$muNorm), 
                                              as.numeric(input$sigNorm)), sep=""))
    }
    if(fNorm() == 2){
      cat(paste("R Code:\n", "pnorm(", input$valNorm, ",", input$muNorm, ",", input$sigNorm, ")",  
                "\n\nResult:\nF(y) = P(Y < y) = ", pnorm(as.numeric(input$valNorm), 
                                                         as.numeric(input$muNorm), 
                                                         as.numeric(input$sigNorm)), sep=""))
    }
    if(fNorm() == 3){
      cat(paste("R Code:\n", "qnorm(", input$valNorm, ",", input$muNorm, ",", input$sigNorm, ")",  
                "\n\nResult:\n y = Q(p) = ", qnorm(as.numeric(input$valNorm), 
                                                   as.numeric(input$muNorm), 
                                                   as.numeric(input$sigNorm)), sep=""))
    }
  })
  
  ###################################################################
  # GAMMA
  
  fGamma <- function(){
    switch(input$fnxGamma, pmf = 1, cdf = 2, quf = 3)
  }
  
  output$valGamma <- renderUI({
    
    if(fGamma() == 1){
      # Input: 
      numericInput(inputId = "valGamma",
                   label = "Value (y)",
                   value = input$alphaGam*input$betaGam,
                   min=0)
    }else if(fGamma() == 2){
      # Input: 
      numericInput(inputId = "valGamma",
                   label = "Value (y)",
                   value = input$alphaGam*input$betaGam,
                   min=0)
    }else if(fGamma() == 3){
      # Input: 
      numericInput(inputId = "valGamma",
                   label = "Value (p)",
                   value = 0.5,
                   min = 0, max = 1)
    }
  })
  
  output$gammaPDF <- renderPlot({
    
    if(fGamma() == 1){
      plot.gamma.pdf(as.numeric(input$alphaGam), as.numeric(input$betaGam), 
                    fnx=1, val=as.numeric(input$valGamma))
    }else if(fGamma() == 2){
      plot.gamma.pdf(as.numeric(input$alphaGam), as.numeric(input$betaGam), 
                    fnx=2, val=as.numeric(input$valGamma))
    }else if(fGamma() == 3){
      plot.gamma.pdf(as.numeric(input$alphaGam), as.numeric(input$betaGam), 
                    fnx=3, val=as.numeric(input$valGamma))
    }
  })
  
  output$gammaCDF <- renderPlot({
    
    if(fGamma() == 1){
      plot.gamma.CDF(as.numeric(input$alphaGam), as.numeric(input$betaGam), 
                    fnx=1, val=as.numeric(input$valGamma))
    }else if(fGamma() == 2){
      plot.gamma.CDF(as.numeric(input$alphaGam), as.numeric(input$betaGam), 
                    fnx=2, val=as.numeric(input$valGamma))
    }else if(fGamma() == 3){
      plot.gamma.CDF(as.numeric(input$alphaGam), as.numeric(input$betaGam), 
                    fnx=3, val=as.numeric(input$valGamma))
    }
  })
  
  # Display the R code and resulting probability ----
  output$gammaR <- renderPrint({
    if(fGamma() == 1){
      cat(paste("R Code:\n", "dgamma(", input$valGamma, ", shape=", input$alphaGam, 
                ", scale=", input$betaGam, ")",  
                "\n\nResult:\nf(y) = ", dgamma(as.numeric(input$valGamma), 
                                              shape=as.numeric(input$alphaGam), 
                                              scale=as.numeric(input$betaGam)), sep=""))
    }
    if(fGamma() == 2){
      cat(paste("R Code:\n", "pgamma(", input$valGamma, ", shape=", input$alphaGam, 
                ", scale=", input$betaGam, ")",  
                "\n\nResult:\nF(y) = P(Y < y) = ", pgamma(as.numeric(input$valGamma), 
                                                         shape=as.numeric(input$alphaGam), 
                                                         scale=as.numeric(input$betaGam)), sep=""))
    }
    if(fGamma() == 3){
      cat(paste("R Code:\n", "qgamma(", input$valGamma, ", shape=", input$alphaGam, 
                ", scale=", input$betaGam, ")",  
                "\n\nResult:\n y = Q(p) = ", qgamma(as.numeric(input$valGamma), 
                                                   shape=as.numeric(input$alphaGam), 
                                                   scale=as.numeric(input$betaGam)), sep=""))
    }
  })
  
  ###################################################################
  # EXPONENTIAL
  
  fExp <- function(){
    switch(input$fnxExp, pmf = 1, cdf = 2, quf = 3)
  }
  
  output$valExp <- renderUI({
    
    if(fExp() == 1){
      # Input: 
      numericInput(inputId = "valExp",
                   label = "Value (y)",
                   value = 1/input$lambda,
                   min=0)
    }else if(fExp() == 2){
      # Input: 
      numericInput(inputId = "valExp",
                   label = "Value (y)",
                   value = 1/input$lambda,
                   min=0)
    }else if(fExp() == 3){
      # Input: 
      numericInput(inputId = "valExp",
                   label = "Value (p)",
                   value = 0.5,
                   min = 0, max = 1)
    }
  })
  
  output$expPDF <- renderPlot({
    
    if(fExp() == 1){
      plot.exp.pdf(as.numeric(input$lambda), fnx=1, val=as.numeric(input$valExp))
    }else if(fExp() == 2){
      plot.exp.pdf(as.numeric(input$lambda), fnx=2, val=as.numeric(input$valExp))
    }else if(fExp() == 3){
      plot.exp.pdf(as.numeric(input$lambda), fnx=3, val=as.numeric(input$valExp))
    }
  })
  
  output$expCDF <- renderPlot({
    
    if(fExp() == 1){
      plot.exp.CDF(as.numeric(input$lambda), fnx=1, val=as.numeric(input$valExp))
    }else if(fExp() == 2){
      plot.exp.CDF(as.numeric(input$lambda), fnx=2, val=as.numeric(input$valExp))
    }else if(fExp() == 3){
      plot.exp.CDF(as.numeric(input$lambda), fnx=3, val=as.numeric(input$valExp))
    }
  })
  
  # Display the R code and resulting probability ----
  output$expR <- renderPrint({
    if(fExp() == 1){
      cat(paste("R Code:\n", "dexp(", input$valExp, ",", input$lambda, ")",  
                "\n\nResult:\nf(y) = ", dexp(as.numeric(input$valExp), 
                                             as.numeric(input$lambda)), sep=""))
    }
    if(fExp() == 2){
      cat(paste("R Code:\n", "pexp(", input$valExp, ",", input$lambda, ")",  
                "\n\nResult:\nF(y) = P(Y < y) = ", pexp(as.numeric(input$valExp),
                                                        as.numeric(input$lambda)), sep=""))
    }
    if(fExp() == 3){
      cat(paste("R Code:\n", "qexp(", input$valExp, ",", input$lambda, ")",  
                "\n\nResult:\n y = Q(p) = ", qexp(as.numeric(input$valExp), 
                                                  as.numeric(input$lambda)), sep=""))
    }
  })
  
  ###################################################################
  # CHI-SQUARED

  fChiSq <- function(){
    switch(input$fnxChiSq, pmf = 1, cdf = 2, quf = 3)
  }
  
  output$valChiSq <- renderUI({
    
    if(fChiSq() == 1){
      # Input: 
      numericInput(inputId = "valChiSq",
                   label = "Value (y)",
                   value = input$nuDF,
                   min=0)
    }else if(fChiSq() == 2){
      # Input: 
      numericInput(inputId = "valChiSq",
                   label = "Value (y)",
                   value = input$nuDF,
                   min=0)
    }else if(fChiSq() == 3){
      # Input: 
      numericInput(inputId = "valChiSq",
                   label = "Value (p)",
                   value = 0.5,
                   min = 0, max = 1)
    }
  })
  
  output$chisqPDF <- renderPlot({
    
    if(fChiSq() == 1){
      plot.chisq.pdf(as.numeric(input$nuDF), fnx=1, val=as.numeric(input$valChiSq))
    }else if(fChiSq() == 2){
      plot.chisq.pdf(as.numeric(input$nuDF), fnx=2, val=as.numeric(input$valChiSq))
    }else if(fChiSq() == 3){
      plot.chisq.pdf(as.numeric(input$nuDF), fnx=3, val=as.numeric(input$valChiSq))
    }
  })
  
  output$chisqCDF <- renderPlot({
    
    if(fChiSq() == 1){
      plot.chisq.CDF(as.numeric(input$nuDF), fnx=1, val=as.numeric(input$valChiSq))
    }else if(fChiSq() == 2){
      plot.chisq.CDF(as.numeric(input$nuDF), fnx=2, val=as.numeric(input$valChiSq))
    }else if(fChiSq() == 3){
      plot.chisq.CDF(as.numeric(input$nuDF), fnx=3, val=as.numeric(input$valChiSq))
    }
  })
  
  # Display the R code and resulting probability ----
  output$chisqR <- renderPrint({
    if(fChiSq() == 1){
      cat(paste("R Code:\n", "dchisq(", input$valChiSq, ",", input$nuDF, ")",  
                "\n\nResult:\nf(y) = ", dchisq(as.numeric(input$valChiSq), 
                                               as.numeric(input$nuDF)), sep=""))
    }
    if(fChiSq() == 2){
      cat(paste("R Code:\n", "pchisq(", input$valChiSq, ",", input$nuDF, ")",  
                "\n\nResult:\nF(y) = P(Y < y) = ", pchisq(as.numeric(input$valChiSq),
                                                          as.numeric(input$nuDF)), sep=""))
    }
    if(fChiSq() == 3){
      cat(paste("R Code:\n", "qchisq(", input$valChiSq, ",", input$nuDF, ")",  
                "\n\nResult:\n y = Q(p) = ", qchisq(as.numeric(input$valChiSq), 
                                                    as.numeric(input$nuDF)), sep=""))
    }
  })
  
  ###################################################################
  # BETA
  
  fBeta <- function(){
    switch(input$fnxBeta, pmf = 1, cdf = 2, quf = 3)
  }
  
  output$valBeta <- renderUI({
    
    if(fBeta() == 1){
      # Input: 
      numericInput(inputId = "valBeta",
                   label = "Value (y)",
                   value = input$alphaBeta/(input$alphaBeta+input$betaBeta),
                   min=0, max=1)
    }else if(fBeta() == 2){
      # Input: 
      numericInput(inputId = "valBeta",
                   label = "Value (y)",
                   value = input$alphaBeta/(input$alphaBeta+input$betaBeta),
                   min=0, max=1)
    }else if(fBeta() == 3){
      # Input: 
      numericInput(inputId = "valBeta",
                   label = "Value (p)",
                   value = 0.5,
                   min = 0, max = 1)
    }
  })
  
  output$betaPDF <- renderPlot({
    
    if(fBeta() == 1){
      plot.beta.pdf(as.numeric(input$alphaBeta), as.numeric(input$betaBeta), 
                     fnx=1, val=as.numeric(input$valBeta))
    }else if(fBeta() == 2){
      plot.beta.pdf(as.numeric(input$alphaBeta), as.numeric(input$betaBeta), 
                     fnx=2, val=as.numeric(input$valBeta))
    }else if(fBeta() == 3){
      plot.beta.pdf(as.numeric(input$alphaBeta), as.numeric(input$betaBeta), 
                     fnx=3, val=as.numeric(input$valBeta))
    }
  })
  
  output$betaCDF <- renderPlot({
    
    if(fBeta() == 1){
      plot.beta.CDF(as.numeric(input$alphaBeta), as.numeric(input$betaBeta), 
                     fnx=1, val=as.numeric(input$valBeta))
    }else if(fBeta() == 2){
      plot.beta.CDF(as.numeric(input$alphaBeta), as.numeric(input$betaBeta), 
                     fnx=2, val=as.numeric(input$valBeta))
    }else if(fBeta() == 3){
      plot.beta.CDF(as.numeric(input$alphaBeta), as.numeric(input$betaBeta), 
                     fnx=3, val=as.numeric(input$valBeta))
    }
  })
  
  # Display the R code and resulting probability ----
  output$betaR <- renderPrint({
    if(fBeta() == 1){
      cat(paste("R Code:\n", "dbeta(", input$valBeta, ",", input$alphaBeta, 
                ", ", input$betaBeta, ")",  
                "\n\nResult:\nf(y) = ", dbeta(as.numeric(input$valBeta), 
                                              as.numeric(input$alphaBeta), 
                                              as.numeric(input$betaBeta)), sep=""))
    }
    if(fBeta() == 2){
      cat(paste("R Code:\n", "pbeta(", input$valBeta, ",", input$alphaBeta, 
                ",", input$betaBeta, ")",  
                "\n\nResult:\nF(y) = P(Y < y) = ", pbeta(as.numeric(input$valBeta), 
                                                        as.numeric(input$alphaBeta), 
                                                        as.numeric(input$betaBeta)), sep=""))
    }
    if(fBeta() == 3){
      cat(paste("R Code:\n", "qbeta(", input$valBeta, ",", input$alphaBeta, 
                ",", input$betaBeta, ")",  
                "\n\nResult:\n y = Q(p) = ", qbeta(as.numeric(input$valBeta), 
                                                  as.numeric(input$alphaBeta), 
                                                  as.numeric(input$betaBeta)), sep=""))
    }
  })
  
}

shinyApp(ui = ui, server = server)