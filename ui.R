shinyUI(fluidPage(
        titlePanel("title panel"),
        
        sidebarLayout(position = "left",
                sidebarPanel(
                        textInput("text", label = h3("Text input"), value = 'illness'),
                        actionButton("goButton","szukaj")),
                mainPanel(
                        textOutput("text1"),
                        tableOutput("table1")
                        )
        )
))