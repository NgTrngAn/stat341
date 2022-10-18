library(shiny)

fluidPage(
    
    titlePanel("STAT 341: Lab #6"),

    tabsetPanel(type="tabs",

        tabPanel(
            h3("Activity 1"),
            tabsetPanel(
                tabPanel(
                    h4("Uniform Distribution"),
                    
                    # sidebar layout with input and output definitions
                    sidebarLayout(
                        sidebarPanel(
                            # input
                            numericInput(
                                inputId = 'lowerUnif',
                                label = "Lower Bound - A",
                                value = 0
                            ),
                            
                            numericInput(
                                inputId = 'upperUnif',
                                label = 'Upper Bound - B',
                                value = 1
                            ),

                            actionButton("plotUnif", "Plot")
                        ),
                        mainPanel(plotOutput("Unif"))
                    )
                ),

                tabPanel(
                    h4("Normal Distribution"),
                    
                    # sidebar layout with input and output definitions
                    sidebarLayout(
                        sidebarPanel(
                            numericInput(
                                inputId = 'muNormal',
                                label = 'Mean - mu',
                                value = 0
                            ),

                            numericInput(
                                inputId = 'sigmaNormal',
                                label = 'Standard Deviation - sigma',
                                value = 1,
                                min = 0
                            ),

                            actionButton("plotNormal", "Plot")
                        ),
                        mainPanel(plotOutput("Normal"))
                    )
                ),

                tabPanel(
                    h4("Gamma Distribution"),
                    
                    # sidebar layout with input and output definitions
                    sidebarLayout(
                        sidebarPanel(
                            numericInput(
                                inputId = 'shapeGamma',
                                label = 'Shape - alpha',
                                value = 1,
                                min = 0
                            ),

                            numericInput(
                                inputId = 'scaleGamma',
                                label = 'Scale - beta',
                                value = 1,
                                min = 0
                            ),

                            actionButton("plotGamma", "Plot")
                        ),

                        mainPanel(plotOutput("Gamma"))
                    )
                ),

                tabPanel(
                    h4("Exponential Distribution"),
                    
                    # sidebar layout with input and output definitions
                    sidebarLayout(
                        sidebarPanel(
                            numericInput(
                                inputId = 'rateExp',
                                label = 'Rate - Lambda',
                                value = 1,
                                min = 0
                            ),

                            actionButton("plotExp", "Plot")
                        ),

                        mainPanel(plotOutput("Exp"))
                    )
                ),

                tabPanel(
                    h4("Chi-square Distribution"),
                    
                    # sidebar layout with input and output definitions
                    sidebarLayout(
                        sidebarPanel(
                            numericInput(
                                inputId = 'dfChisq',
                                label = 'Degrees of freedom - nu',
                                value = 1,
                                min = 0
                            ),

                            actionButton("plotChisq", "Plot")
                        ),

                        mainPanel(plotOutput("Chisq"))
                    )
                ),

                tabPanel(
                    h4("Beta Distribution"),
                    
                    # sidebar layout with input and output definitions
                    sidebarLayout(
                        sidebarPanel(
                            numericInput(
                                inputId = 'shapeBeta',
                                label = 'Shape - alpha',
                                value = 1,
                                min = 0
                            ),

                            numericInput(
                                inputId = 'scaleBeta',
                                label = 'Scale - Beta',
                                value = 1,
                                min = 0
                            ),

                            actionButton("plotBeta", "Plot")
                        ),

                        mainPanel(plotOutput("Beta"))
                    )
                )
            )
        ),

        tabPanel(
            h3("Activity 2"),
            tabsetPanel(
                tabPanel(
                    h4("Data Summary"),
                    verbatimTextOutput("randuSummary")
                ),

                tabPanel(
                    h4("Histograms"),
                    sidebarLayout(
                        sidebarPanel(
                            # input
                            radioButtons(
                                inputId = "randuVarChoice",
                                label = "Selecting Variable",
                                choices = c("X" = "randux", "Y" = "randuy", "Z" = "randuz")
                            )
                        ),
                        mainPanel(plotOutput("randuHistogram"))
                    )
                ),

                tabPanel(
                    h4("QQ Plot"),

                    sidebarLayout(
                        sidebarPanel(
                            selectInput(
                                inputId = "randuEmp",
                                label = "Empirical Data",
                                choices = c("X" = "randuempx", 
                                            "Y" = "randuempy", 
                                            "Z" = "randuempz")
                            ),

                            selectInput(
                                inputId = "theoreticalDist",
                                label = "Theoretical Distribution",
                                choices = c("Beta" = "beta",
                                            "Normal" = "norm", 
                                            "Chi Square" = "chisq", 
                                            "Exponential" = "exp",
                                            "Gamma" = "gamma",
                                            "Uniform" = "unif")
                            ),

                            conditionalPanel(
                                condition = "input.theoreticalDist == 'beta'",
                                numericInput(
                                inputId = 'qqShapeBeta',
                                label = 'Shape - alpha',
                                value = 1,
                                min = 0
                                ),

                                numericInput(
                                    inputId = 'qqScaleBeta',
                                    label = 'Scale - beta',
                                    value = 1,
                                    min = 0
                                )
                            ),

                            conditionalPanel(
                                condition = "input.theoreticalDist == 'norm'",
                                numericInput(
                                inputId = 'qqMuNormal',
                                label = 'Mean',
                                value = 0
                                ),

                                numericInput(
                                    inputId = 'qqSigmaNormal',
                                    label = 'Sigma',
                                    value = 1,
                                    min = 0
                                )
                            ),

                            conditionalPanel(
                                condition = "input.theoreticalDist == 'chisq'",
                                numericInput(
                                inputId = 'qqDfChisq',
                                label = 'Degrees of freedom',
                                value = 1,
                                min = 0
                                )
                            ),

                            conditionalPanel(
                                condition = "input.theoreticalDist == 'exp'",
                                numericInput(
                                inputId = 'qqRateExp',
                                label = 'Rate',
                                value = 1,
                                min = 0
                                )
                            ),

                            conditionalPanel(
                                condition = "input.theoreticalDist == 'gamma'",
                                numericInput(
                                inputId = 'qqShapeGamma',
                                label = 'Shape - alpha',
                                value = 1,
                                min = 0
                                ),

                                numericInput(
                                    inputId = 'qqScaleGamma',
                                    label = 'Scale - beta',
                                    value = 1,
                                    min = 0
                                )
                            ),

                            conditionalPanel(
                                condition = "input.theoreticalDist == 'unif'",
                                numericInput(
                                inputId = 'qqLowerUnif',
                                label = 'Lower Bound',
                                value = 0
                                ),

                                numericInput(
                                    inputId = 'qqUpperUnif',
                                    label = 'Upper Bound',
                                    value = 1
                                )
                            ),

                            actionButton("plotqq", "Plot")
                        ),

                        mainPanel(plotOutput("qqplot"))
                    )
                )
            )
        ),

        tabPanel(
            h3("Activity 3")
        ),

        tabPanel(
            h3("Activity 4")
        )
    )
)