## manipulate
library(manipulate)
myPlot <- function(s) {
        plot(cars$dist - mean(cars$dist), cars$speed - mean(cars$speed))
        abline(0, s)
}
manipulate(myPlot(s), s = slider(0, 2, step = 0.1))



## rCharts
require(devtools)
install_github('rCharts', 'ramnathv/rCharts/rCharts')
require(rCharts)
dTable(airquality, sPaginationType = "full_numbers")


library(shiny)
shinyUI(pageWithSidebar(  
        headerPanel("Data science FTW!"),  
        sidebarPanel(    
                h2('Big text'),    
                h3('Sidebar')  
        ),  
        mainPanel(      
                h3('Main Panel text')  
        )
))


shinyUI(pageWithSidebar(  
        headerPanel("Example plot"),  
        sidebarPanel(    
                sliderInput('mu', 'Guess at the mu',value = 70, min = 60, max = 80, step = 0.05,)  ), 
        mainPanel(    
                plotOutput('newHist')  
        )
))

library(UsingR)
data(galton)

shinyServer(  
        function(input, output) {    
                output$myHist <- renderPlot({      
                        hist(galton$child, xlab='child height', col='lightblue',main='Histogram')      
                        mu <- input$mu      
                        lines(c(mu, mu), c(0, 200),col="red",lwd=5)      
                        mse <- mean((galton$child - mu)^2)      
                        text(63, 150, paste("mu = ", mu))      
                        text(63, 140, paste("MSE = ", round(mse, 2)))      
                })      }
)

