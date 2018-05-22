library(shiny)
library(SnowballC)
library(qdap)
library(ggplot2)
library(RISmed)
library(wordcloud)

shinyServer(function(input, output) {
        word1<-eventReactive(input$goButton, {input$text})
        output$distPlot <- renderPlot({
                d1<-input$date1
                d2<-input$date2
                
                res <- EUtilsSummary(word1(), type="esearch", db="pubmed", datetype='pdat', mindate=d1, maxdate=d2, retmax=500)
                date()
                fetch <- EUtilsGet(res, type="efetch", db="pubmed")
                count<-table(YearPubmed(fetch))
                count<-as.data.frame(count)
                names(count)<-c("Year", "Counts")
                num <- data.frame(Year=count$Year, Counts=cumsum(count$Counts)) 
                num$g <- "g"
                names(num) <- c("Year", "Counts", "g")
                
                q <- qplot(x=Year, y=Counts, data=count, geom="bar", stat="identity")
                q <- q + geom_line(aes(x=Year, y=Counts, group=g), data=num) +
                        ggtitle(paste("PubMed articles containing '", word1(), "' ", "= ", max(num$Counts), sep="")) +
                        ylab("Number of articles") +
                        xlab(paste("Year n Query date: ", Sys.time(), sep="")) +
                        labs(colour="") + theme_bw()
                
                q
        })
        
        word2<-eventReactive(input$wordButton, {input$text})
        output$wordPlot<-renderPlot({
                d1<-input$date1
                d2<-input$date2
                
                res <- EUtilsSummary(word2(), type="esearch", db="pubmed", datetype='pdat', mindate=d1, maxdate=d2, retmax=500)
                fetch <- EUtilsGet(res, type="efetch", db="pubmed")
                articles<-data.frame('Abstract'=AbstractText(fetch))
                abstracts<-as.character(articles$Abstract)
                abstracts<-paste(abstracts, sep="", collapse="") 
                wordcloud(abstracts, min.freq=10, max.words=70, colors=brewer.pal(7,"Dark2"))
        })
        
        word3<-eventReactive(input$authButton, {input$text})
        output$authList<-renderTable({
                d1<-input$date1
                d2<-input$date2
                
                res <- EUtilsSummary(word3(), type="esearch", db="pubmed", 
                                     datetype='pdat', mindate=d1, maxdate=d2, retmax=500)
                fetch <- EUtilsGet(res, type="efetch", db=" pubmed")
                AuthorList<-Author(fetch)
                Last<-sapply(AuthorList, function(x)paste(x$LastName))
                auths<-as.data.frame(sort(table(unlist(Last)), dec=TRUE))
                colnames(auths)<-"Count"
                auths <- cbind(Author = rownames(auths), auths)
                rownames(auths) <- NULL
                
                auths<-head(auths, 6)
        })
})