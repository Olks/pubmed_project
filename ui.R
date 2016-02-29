library(shiny) 

shinyUI(pageWithSidebar( 
        headerPanel("PubMed Articles Browser"), 
        sidebarPanel( 
                textInput(inputId = "term", 
                          label = "Enter searched term", 
                          value = "" 
                ),
                actionButton("searchButton","Search"),
                
                actionButton("sortButton","Relevance sort"),
                
                textInput(inputId = "bestArticles", 
                          label = "Number of first most relevant articles to analyse", 
                          value = "" 
                ),
                
                actionButton("bestButton","Choose")
                
        ),
        mainPanel( 
                h2("Results"), 
                h5("It may take a while ..."),
                textOutput("articlesNr"),
                tableOutput("authorsTable"),
                plotOutput("relevancePlot")
        )
))