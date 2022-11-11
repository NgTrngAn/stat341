library(shiny)

fluidPage(
    
    titlePanel("STAT 341: Lab #9"),

    tabsetPanel(type="tabs",

        tabPanel(
            h3("Activity 1 - Simplified Dice"),

            sidebarLayout(
                sidebarPanel(
                    numericInput(
                        inputId = 'trialsNum',
                        label = "Number of trials (Number of times to roll this dice)",
                        value = 100
                    ),

                    actionButton(
                        inputId = 'roll',
                        label = "Roll"
                    ),


                ),
                mainPanel(
                    h4("Results of the first 30 rolls"),

                    tableOutput("table1"),

                    plotOutput("plot1")
                )
            )
        )
    )
)