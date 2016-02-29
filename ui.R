library(shiny) 



shinyUI(pageWithSidebar( 
        headerPanel("PubMed Articles Browser"), 
        sidebarPanel( 
                textInput(inputId = "term", 
                          label = "Enter searched term:", 
                          value = "" 
                ),
                actionButton("searchButton","Search"),
                h4("--------------------------------"),
                h5(strong("Sort the results by their relevance:")),
                actionButton("sortButton","Relevance plot"),
                h4("--------------------------------"),
                textInput(inputId = "bestArticlesNum", 
                          label = "Number of first most relevant articles 
                                        to analyse", 
                          value = "" 
                ),
                
                actionButton("bestButton","Choose")
                
        ),
        mainPanel( 
                h2("Results"), 
                h5("It may take a while ..."),
                textOutput("articlesNr"),
                h2(" "),
                #tableOutput("authorsTable"),
                plotOutput("relevancePlot"),
                tableOutput("bestArticles")
        )
))