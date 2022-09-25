library(shiny)
library(ggplot2)
library(extraDistr)
#runApp("~/Box Sync/STAT-341-DMO/Fall2022/Homework/DiscreteProb_shiny")
# @author: Dr. Danica Ommen

# Define UI for app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("STAT 341: Computing Discrete Probabilities"),
  
  # Output: Tabset for different Activities ----
  tabsetPanel(type = "tabs",
              tabPanel("binom", 
                       h3("Binomial Distribution"),
                       
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Numeric entry for choosing Number of Trials (n) ----
                           numericInput(inputId = "nBinom",
                                        label = "Number of trials (n)",
                                        value = 1,
                                        min = 1),
                           # Input: Numeric entry for probability of success (p) ----
                           numericInput(inputId = "pBinom",
                                        label = "Probability of success (p)",
                                        value = 0.5,
                                        min = 0, max = 1),
                           # Input: Radio button to select PMF or CDF probability
                           radioButtons("fnxBinom", "Probability function:",
                                        c("PMF" = "pmf",
                                          "CDF" = "cdf")),
                           # Input: Numeric entry for function evaluation value (y) ----
                           uiOutput("valBinom")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Probability Histogram for Binomial RV ----
                           plotOutput("BinomPlot"),
                           # Output: Verbatim text for R code & results ----
                           verbatimTextOutput("BinomR")
                         )
                       )),
              tabPanel("geom", 
                       h3("Geometric Distributions"),
                       
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Numeric entry for probability of success (p) ----
                           numericInput(inputId = "pGeom",
                                        label = "Probability of success (p)",
                                        value = 0.5,
                                        min = 0, max = 1),
                           # Input: Radio button to select PMF or CDF probability
                           radioButtons("fnxGeom", "Probability function:",
                                        c("PMF" = "pmf",
                                          "CDF" = "cdf")),
                           # Input: Numeric entry for function evaluation value (y) ----
                           numericInput(inputId = "valGeom",
                                        label = "Value (y = # trials until 1st success)",
                                        value = 1,
                                        min = 1)
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Probability Histogram for Geometric RV ----
                           plotOutput("GeomPlot"),
                           # Output: Verbatim text for R code & results ----
                           verbatimTextOutput("GeomR")
                         )
                       )),
              tabPanel("nbinom", 
                       h3("Negative Binomial Distributions"),
                       
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Numeric entry for choosing Number of successes (r) ----
                           numericInput(inputId = "rNegBin",
                                        label = "Number of successes (r)",
                                        value = 1,
                                        min = 1),
                           # Input: Numeric entry for probability of success (p) ----
                           numericInput(inputId = "pNegBin",
                                        label = "Probability of success (p)",
                                        value = 0.5,
                                        min = 0, max = 1),
                           # Input: Radio button to select PMF or CDF probability
                           radioButtons("fnxNegBin", "Probability function:",
                                        c("PMF" = "pmf",
                                          "CDF" = "cdf")),
                           # Input: Numeric entry for function evaluation value (y) ----
                           uiOutput("valNegBin")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Probability Histogram for Negative Binomial RV ----
                           plotOutput("NegBinPlot"),
                           # Output: Verbatim text for R code & results ----
                           verbatimTextOutput("NegBinR")
                         )
                       )),
              tabPanel("hyper", 
                       h3("Hypergeometric Distributions"),
                       
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Numeric entry for choosing Population size (N) ----
                           numericInput(inputId = "NHyper",
                                        label = "Population size (N)",
                                        value = 1,
                                        min = 1),
                           # Input: Numeric entry for choosing Number of population successes (M) ----
                            numericInput(inputId = "MHyper",
                                        label = "Population successes (M)",
                                        value = 1,
                                        min=1),
                           # Input: Numeric entry for choosing Sample Size (n) ----
                            numericInput(inputId = "nHyper",
                                        label = "Sample Size (n)",
                                        value = 1,
                                        min=1),
                           # Input: Radio button to select PMF or CDF probability
                           radioButtons("fnxHyper", "Probability function:",
                                        c("PMF" = "pmf",
                                          "CDF" = "cdf")),
                           # Input: Numeric entry for function evaluation value (y) ----
                           uiOutput("valHyper")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Probability Histogram for Hypergeometric RV ----
                           plotOutput("HyperPlot"),
                           # Output: Verbatim text for R code & results ----
                           verbatimTextOutput("HyperR")
                         )
                       )),
              tabPanel("nhyper",
                       h3("Negative Hypergeometric Distributions"),

                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Numeric entry for choosing Population size (N) ----
                           numericInput(inputId = "NnegHyp",
                                        label = "Population size (N)",
                                        value = 2,
                                        min = 2),
                           # Input: Numeric entry for choosing Number of population successes (M) ----
                           numericInput(inputId = "MnegHyp",
                                        label = "Population successes (M)",
                                        value = 1,
                                        min=1),
                           # Input: Numeric entry for choosing Number of successes (r) ----
                           numericInput(inputId = "rNegHyp",
                                        label = "Number of successes (r)",
                                        value = 1,
                                        min=1),
                           # Input: Radio button to select PMF or CDF probability
                           radioButtons("fnxNegHyp", "Probability function:",
                                        c("PMF" = "pmf",
                                          "CDF" = "cdf")),
                           # Input: Numeric entry for function evaluation value (y) ----
                           uiOutput("valNegHyp")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Probability Histogram for Hypergeometric RV ----
                           plotOutput("NegHypPlot"),
                           # Output: Verbatim text for R code & results ----
                           verbatimTextOutput("NegHypR")
                         )
                       )),
              tabPanel("pois",
                       h3("Poisson Distribution"),

                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Numeric entry for choosing Rate per unit (lambda) ----
                           numericInput(inputId = "lambda",
                                        label = "Rate per unit (lambda)",
                                        value = 1,
                                        min = 0),
                           # Input: Radio button to select PMF or CDF probability
                           radioButtons("fnxPois", "Probability function:",
                                        c("PMF" = "pmf",
                                          "CDF" = "cdf")),
                           # Input: Numeric entry for function evaluation value (y) ----
                           numericInput(inputId = "valPois",
                                        label = "Value (y = Number of events)",
                                        value = 0,
                                        min = 0),
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Probability Histogram for Hypergeometric RV ----
                           plotOutput("PoisPlot"),
                           # Output: Verbatim text for R code & results ----
                           verbatimTextOutput("PoisR")
                         )
                       ))
  )
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  ###################################################################
  # BINOMIAL
  
  output$valBinom <- renderUI({
    # Input: Numeric entry for function evaluation value (y) ----
    numericInput(inputId = "valBinom",
                 label = "Value (y = number of successes)",
                 value = 0,
                 min = 0, max = input$nBinom)
  })
  
  fBinom <- function(){
    switch(input$fnxBinom, pmf = 1, cdf = 2)
  }
  
  # display probability histogram for the Binomial distribution
  output$BinomPlot <- renderPlot({
    
    y<- c(0:input$nBinom)
    proby<- dbinom(0:input$nBinom, input$nBinom, input$pBinom)
    Bars<- as.data.frame(cbind(y, proby))
    fills<- rep("blue", length(y))
    
    if(fBinom() == 1){
      fills[which(y == input$valBinom)] = "red"
    }
    if(fBinom() == 2){
      fills[which(y <= input$valBinom)] = "red"
    }
    
    ggplot(Bars, aes(x = y, y = proby))+ 
      geom_bar(stat="identity", width = 1, fill = fills, colour = "black")+
      labs(x = "y", y = "p(y)")+
      theme_bw()+
      theme(axis.title.y = element_text(size = rel(1.4)),
            axis.title.x = element_text(size = rel(1.4)),
            axis.text.x = element_text(size = rel(1.2)),
            axis.text.y = element_text(size = rel(1.2)),
            plot.title = element_text(hjust=0.5, size = rel(1.75)),
            plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))
  })
  
  # Display the R code and resulting probability ----
  output$BinomR <- renderPrint({
    if(fBinom() == 1){
      cat(paste("R Code:\n", "dbinom(", input$valBinom, ",", input$nBinom, ",", input$pBinom, ")",  
                "\n\nResult:\np(y) = P(Y=y) = ", dbinom(as.numeric(input$valBinom), as.numeric(input$nBinom), input$pBinom), sep=""))
    }
    if(fBinom() == 2){
      cat(paste("R Code:\n", "pbinom(", input$valBinom, ",", input$nBinom, ",", input$pBinom, ")",  
                "\n\nResult:\nF(y) = P(Y<=y) = ", pbinom(input$valBinom, input$nBinom, input$pBinom), sep=""))
    }
  })
  
  
  ###################################################################
  # GEOMETRIC
  
  fGeom <- function(){
    switch(input$fnxGeom, pmf = 1, cdf = 2)
  }
  
  # display probability histogram for the Geometric distribution
  output$GeomPlot <- renderPlot({
    
    y<- c(0:qgeom(0.9999, input$pGeom))
    proby<- dgeom(y, input$pGeom)
    Bars<- as.data.frame(cbind(y, proby))
    fills<- rep("blue", length(y))
    
    if(fGeom() == 1){
      fills[which(y+1 == input$valGeom)] = "red"
    }
    if(fGeom() == 2){
      fills[which(y+1 <= input$valGeom)] = "red"
    }
    
    ggplot(Bars, aes(x = y+1, y = proby))+ 
      geom_bar(stat="identity", width = 1, fill = fills, colour = "black")+
      labs(x = "y", y = "p(y)")+
      theme_bw()+
      theme(axis.title.y = element_text(size = rel(1.4)),
            axis.title.x = element_text(size = rel(1.4)),
            axis.text.x = element_text(size = rel(1.2)),
            axis.text.y = element_text(size = rel(1.2)),
            plot.title = element_text(hjust=0.5, size = rel(1.75)),
            plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))
  })
  
  # Display the R code and resulting probability ----
  output$GeomR <- renderPrint({
    if(fGeom() == 1){
      cat(paste("R Code:\n", "dgeom(", input$valGeom, "-1,", input$pGeom, ")",  
                "\n\nResult:\np(y) = P(Y=y) = ", dgeom(input$valGeom-1, input$pGeom), sep=""))
    }
    if(fGeom() == 2){
      cat(paste("R Code:\n", "pgeom(", input$valGeom, "-1,", input$pGeom, ")",  
                "\n\nResult:\nF(y) = P(Y<=y) = ", pgeom(input$valGeom-1, input$pGeom), sep=""))
    }
  })
  
  
  ###################################################################
  # NEGATIVE BINOMIAL
  
  output$valNegBin <- renderUI({
    # Input: Numeric entry for function evaluation value (y) ----
    numericInput(inputId = "valNegBin",
                 label = "Value (y = number of trials to get r succcesses)",
                 value = input$rNegBin,
                 min = input$rNegBin)
  })
  
  fNegBin <- function(){
    switch(input$fnxNegBin, pmf = 1, cdf = 2)
  }
  
  # display probability histogram for the Geometric distribution
  output$NegBinPlot <- renderPlot({
    
    y<- c(0:qnbinom(0.9999, input$rNegBin, input$pNegBin))
    proby<- dnbinom(y, input$rNegBin, input$pNegBin)
    Bars<- as.data.frame(cbind(y, proby))
    fills<- rep("blue", length(y))
    
    if(fNegBin() == 1){
      fills[which(y+input$rNegBin == input$valNegBin)] = "red"
    }
    if(fNegBin() == 2){
      fills[which(y+input$rNegBin <= input$valNegBin)] = "red"
    }
    
    ggplot(Bars, aes(x = y+input$rNegBin, y = proby))+ 
      geom_bar(stat="identity", width = 1, fill = fills, colour = "black")+
      labs(x = "y", y = "p(y)")+
      theme_bw()+
      theme(axis.title.y = element_text(size = rel(1.4)),
            axis.title.x = element_text(size = rel(1.4)),
            axis.text.x = element_text(size = rel(1.2)),
            axis.text.y = element_text(size = rel(1.2)),
            plot.title = element_text(hjust=0.5, size = rel(1.75)),
            plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))
  })
  
  # Display the R code and resulting probability ----
  output$NegBinR <- renderPrint({
    if(fNegBin() == 1){
      cat(paste("R Code:\n", "dnbinom(", input$valNegBin, "-", input$rNegBin, ",", input$rNegBin, ",", input$pNegBin, ")",  
                "\n\nResult:\np(y) = P(Y=y) = ", dnbinom(input$valNegBin-input$rNegBin, input$rNegBin, input$pNegBin), sep=""))
    }
    if(fNegBin() == 2){
      cat(paste("R Code:\n", "pnbinom(", input$valNegBin, "-", input$rNegBin, ",", input$rNegBin, ",", input$pNegBin, ")",  
                "\n\nResult:\nF(y) = P(Y<=y) = ", pnbinom(input$valNegBin-input$rNegBin, input$rNegBin, input$pNegBin), sep=""))
    }
  })
  
  
  ###################################################################
  # HYPERGEOMETRIC
  
  output$valHyper <- renderUI({
    # Input: Numeric entry for function evaluation value (y) ----
    numericInput(inputId = "valHyper",
                 label = "Value (y = number of successes)",
                 value = 0,
                 min = max(0, input$nHyper-(input$NHyper-input$MHyper)), max = min(input$nHyper, input$MHyper))
  })
  
  fHyper <- function(){
    switch(input$fnxHyper, pmf = 1, cdf = 2)
  }
  
  # display probability histogram for the Binomial distribution
  output$HyperPlot <- renderPlot({
    
    miny<- max(0, input$nHyper-(input$NHyper-input$MHyper))
    maxy<- min(input$nHyper, input$MHyper)
    y<- c(miny:maxy)
    proby<- dhyper(y, input$MHyper, input$NHyper-input$MHyper, input$nHyper)
    Bars<- as.data.frame(cbind(y, proby))
    fills<- rep("blue", length(y))
    
    if(fHyper() == 1){
      fills[which(y == input$valHyper)] = "red"
    }
    if(fHyper() == 2){
      fills[which(y <= input$valHyper)] = "red"
    }
    
    ggplot(Bars, aes(x = y, y = proby))+ 
      geom_bar(stat="identity", width = 1, fill = fills, colour = "black")+
      labs(x = "y", y = "p(y)")+
      theme_bw()+
      theme(axis.title.y = element_text(size = rel(1.4)),
            axis.title.x = element_text(size = rel(1.4)),
            axis.text.x = element_text(size = rel(1.2)),
            axis.text.y = element_text(size = rel(1.2)),
            plot.title = element_text(hjust=0.5, size = rel(1.75)),
            plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))
  })
  
  # Display the R code and resulting probability ----
  output$HyperR <- renderPrint({
    if(fHyper() == 1){
      cat(paste("R Code:\n", "dhyper(", input$valHyper, ",", input$MHyper, ",", input$NHyper, "-", input$MHyper, ",", input$nHyper, ")",  
                "\n\nResult:\np(y) = P(Y=y) = ", dhyper(as.numeric(input$valHyper), as.numeric(input$MHyper), 
                                                        as.numeric(input$NHyper)-as.numeric(input$MHyper), as.numeric(input$nHyper)), sep=""))
    }
    if(fHyper() == 2){
      cat(paste("R Code:\n", "phyper(", input$valHyper, ",", input$MHyper, ",", input$NHyper, "-", input$MHyper, ",", input$nHyper, ")",  
                "\n\nResult:\nF(y) = P(Y<=y) = ", phyper(input$valHyper, input$MHyper, input$NHyper-input$MHyper, input$nHyper), sep=""))
    }
  })
  
  
  ###################################################################
  # NEGATIVE HYPERGEOMETRIC

  output$valNegHyp <- renderUI({
    # Input: Numeric entry for function evaluation value (y) ----
    numericInput(inputId = "valNegHyp",
                 label = "Value (y = number of samples to get r successes)",
                 value = input$rNegHyp,
                 min = input$rNegHyp, max = input$NnegHyp)
  })

  fNegHyp <- function(){
    switch(input$fnxNegHyp, pmf = 1, cdf = 2)
  }

  # display probability histogram for the Binomial distribution
  output$NegHypPlot <- renderPlot({

    y<- c(0:(qnhyper(0.9999, input$NnegHyp-input$MnegHyp, input$MnegHyp, input$rNegHyp)))
    proby<- dnhyper(y+input$rNegHyp, input$NnegHyp-input$MnegHyp, input$MnegHyp, input$rNegHyp)
    Bars<- as.data.frame(cbind(y, proby))
    fills<- rep("blue", length(y))

    if(fNegHyp() == 1){
      fills[which(y+input$rNegHyp == input$valNegHyp)] = "red"
    }
    if(fNegHyp() == 2){
      fills[which(y+input$rNegHyp <= input$valNegHyp)] = "red"
    }

    ggplot(Bars, aes(x = y+input$rNegHyp, y = proby))+
      geom_bar(stat="identity", width = 1, fill = fills, colour = "black")+
      labs(x = "y", y = "p(y)")+
      theme_bw()+
      theme(axis.title.y = element_text(size = rel(1.4)),
            axis.title.x = element_text(size = rel(1.4)),
            axis.text.x = element_text(size = rel(1.2)),
            axis.text.y = element_text(size = rel(1.2)),
            plot.title = element_text(hjust=0.5, size = rel(1.75)),
            plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))
  })

  # Display the R code and resulting probability ----
  output$NegHypR <- renderPrint({
    if(fNegHyp() == 1){
      cat(paste("R Code:\n", "dnhyper(", input$valNegHyp, ",", input$NnegHyp, "-", input$MnegHyp, ",", input$MnegHyp, ",", input$rNegHyp, ")", 
                "\n\nResult:\np(y) = P(Y=y) = ", dnhyper(input$valNegHyp, input$NnegHyp-input$MnegHyp, input$MnegHyp, input$rNegHyp), sep=""))
    }
    if(fNegHyp() == 2){
      cat(paste("R Code:\n", "phyper(", input$valNegHyp, ",", input$NnegHyp, "-", input$MnegHyp, ",", input$MnegHyp, ",", input$rNegHyp, ")", 
                "\n\nResult:\nF(y) = P(Y<=y) = ", pnhyper(input$valNegHyp, input$NnegHyp-input$MnegHyp, input$MnegHyp, input$rNegHyp), sep=""))
    }
  })
  
  
  ###################################################################
  # POISSON
  
  fPois <- function(){
    switch(input$fnxPois, pmf = 1, cdf = 2)
  }
  
  # display probability histogram for the Geometric distribution
  output$PoisPlot <- renderPlot({
    
    y<- c(0:qpois(0.9999, input$lambda))
    proby<- dpois(y, input$lambda)
    Bars<- as.data.frame(cbind(y, proby))
    fills<- rep("blue", length(y))
    
    if(fPois() == 1){
      fills[which(y == input$valPois)] = "red"
    }
    if(fPois() == 2){
      fills[which(y <= input$valPois)] = "red"
    }
    
    ggplot(Bars, aes(x = y, y = proby))+ 
      geom_bar(stat="identity", width = 1, fill = fills, colour = "black")+
      labs(x = "y", y = "p(y)")+
      theme_bw()+
      theme(axis.title.y = element_text(size = rel(1.4)),
            axis.title.x = element_text(size = rel(1.4)),
            axis.text.x = element_text(size = rel(1.2)),
            axis.text.y = element_text(size = rel(1.2)),
            plot.title = element_text(hjust=0.5, size = rel(1.75)),
            plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))
  })
  
  # Display the R code and resulting probability ----
  output$PoisR <- renderPrint({
    if(fPois() == 1){
      cat(paste("R Code:\n", "dpois(", input$valPois, ",", input$lambda, ")",  
                "\n\nResult:\np(y) = P(Y=y) = ", dpois(input$valPois, input$lambda), sep=""))
    }
    if(fPois() == 2){
      cat(paste("R Code:\n", "ppois(", input$valPois, ",", input$lambda, ")",  
                "\n\nResult:\nF(y) = P(Y<=y) = ", ppois(input$valPois, input$lambda), sep=""))
    }
  })
  
}

shinyApp(ui = ui, server = server)
