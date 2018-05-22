library(shiny)
library(shinythemes)
shinyUI(fluidPage(theme=shinytheme("united"),
                  headerPanel("PubMed Search"),
                  sidebarLayout(
                          sidebarPanel(
                                  helpText("Type a word below and search PubMed to find documents that contain that word in the text. You can even type multiple words. You can search authors, topics, any acronym, etc."),
                                  textInput("text", label = h3("Keyord(s)"), value = "carson chow"),
                                  helpText("You can specify the start and end dates of your search, use the format YYYY/MM/DD"),
                                  textInput("date1", label = h3("From"),value="1990/01/01"),
                                  textInput("date2", label = h3("To"),  value = "2015/11/07"),
                                  helpText("Now select the output you'd like to see. You can see a barplot of articles per year, a wordcloud of the abstract texts, or a table of the top six authors"),
                                  
                                  actionButton("goButton","PLOT"),
                                  actionButton("wordButton","WORDS"),
                                  actionButton("authButton","AUTHORS")),
                          
                          mainPanel(
                                  plotOutput("distPlot"),
                                  plotOutput("wordPlot"),
                                  tableOutput("authList")
                                  
                          )
                  )))