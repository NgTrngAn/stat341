library(shiny)

fluidPage(
    
    titlePanel("STAT 341: Lab #5"),

    tabsetPanel(type="tabs",
        tabPanel(
            "Activity #1",
            h3("Hypergeometric Distribution"),
            
            # sidebar layout with input and output definitions
            sidebarLayout(
                sidebarPanel(
                    # input
                    numericInput(
                        inputId = 'mHyper',
                        label = "M - Pop. Success",
                        value = 1,
                        min = 0
                    ),
                    
                    numericInput(
                        inputId = 'NHyper',
                        label = 'N - Pop. Size',
                        value = 1,
                        min = 0
                    ),
                    
                    numericInput(
                        inputId = 'drawHyper',
                        label = 'n - Sample Size',
                        value = 1,
                        min = 0
                    ),

                    actionButton("plotHyperGeom", "Plot")
                ),
                mainPanel(plotOutput("HyperGeom"))
            )
        ),

        tabPanel(
            "Activity #2",
            h3("Negative Hypergeometric Distribution"),
            
            # sidebar layout with input and output definitions
            sidebarLayout(
                sidebarPanel(
                    numericInput(
                        inputId = 'mNegHyper',
                        label = 'M - Pop. Success',
                        value = 1,
                        min = 0
                    ),

                    numericInput(
                        inputId = 'NNegHyper',
                        label = 'N - Pop. Size',
                        value = 1,
                        min = 0
                    ),

                    numericInput(
                        inputId = 'rNegHyper',
                        label = 'r - Num. Success',
                        value = 1,
                        min = 0
                    ),

                    actionButton("plotNegHyperGeom", "Plot")
                ),
                mainPanel(plotOutput("NegHyperGeom"))
            )
        ),

        tabPanel(
            "Activity #3",
            h3("Poisson Distribution"),
            
            # sidebar layout with input and output definitions
            sidebarLayout(
                sidebarPanel(
                    numericInput(
                        inputId = 'rate',
                        label = 'Rate',
                        value = 1,
                        min = 0
                    ),

                    actionButton("plotPoisson", "Plot")
                ),

                mainPanel(plotOutput("Poisson"))
            )
        )
    )
)