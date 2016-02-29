library(shiny) 

shinyUI(pageWithSidebar( 
        headerPanel("PubMed Articles Browser"), 
        sidebarPanel( 
                textInput(inputId = "term", 
                          label = "searched term", 
                          value = "" 
                ),
                actionButton("searchButton","Search")
        ),
        mainPanel( 
                h2("Results"), 
                h5("It may take a while ..."),
                textOutput("articlesNr"),
                tableOutput("authorsTable")
        )
))