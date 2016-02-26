library(RISmed)
shinyServer(function(input, output) {
        word1<-eventReactive(input$goButton, {input$text})
        
        output$text1 <- renderText({ 
                search_topic <- word1()
                search_query <- EUtilsSummary(search_topic)
                # nie wiem czemu - nie moze byc retmax
                x <- QueryCount(search_query)
                paste("There are ", x, "articles about", input$text)
        })
        output$table1 <- renderTable({
                search_topic <- word1()
                search_query <- EUtilsSummary(search_topic)
                # nie wiem czemu - nie moze byc retmax
                records<- EUtilsGet(search_query)
                AuthorList<-Author(records)
                eachAuthor <- function(x){
                        authors<-c()
                        for (i in 1:dim(x)[1]){
                                authors[i]<-paste(x$LastName[i],x$ForeName[i],x$Initials[i])
                                authors <- authors[authors!="NA NA NA"]
                        }
                        return(authors)
                        
                }
                Last<-sapply(AuthorList, eachAuthor)
                auths<-as.data.frame(sort(table(unlist(Last)), dec=TRUE))
                colnames(auths)<-"Count"
                auths <- cbind(Author = rownames(auths), auths)
                rownames(auths) <- NULL
                head(auths,20)
        })
        
}
)