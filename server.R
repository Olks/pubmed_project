library(shiny) 
library(RISmed)

shinyServer(function(input, output) { 
        
        search_topic <- eventReactive(input$searchButton, {input$term})
        
        output$articlesNr <- renderText({
                search_query <- EUtilsSummary(search_topic(), retmax=4000) #type="esearch", db="pubmed", datetype='pdat') #mindate=2014, maxdate=2016)
                N <- QueryCount(search_query)
                paste0("There are ", N, " articles referring to <", search_topic(), ">" )
        })
        
        
        output$authorsTable <- renderTable({
                search_query <- EUtilsSummary(search_topic(), retmax=4000) #type="esearch", db="pubmed", datetype='pdat') #mindate=2014, maxdate=2016)
                fetch <- EUtilsGet(search_query)
                AuthorList<-Author(fetch)
                
                eachAuthor <- function(x){
                        authors<-c()
                        for (i in 1:dim(x)[1]){
                                authors[i]<-paste(x$LastName[i],x$ForeName[i],x$Initials[i])
                        }
                        return(authors)
                }
                
                Last<-sapply(AuthorList, eachAuthor)
                auths<-as.data.frame(sort(table(unlist(Last)), dec=TRUE))
                colnames(auths)<-"Count"
                auths <- cbind(Author = rownames(auths), auths)
                rownames(auths) <- NULL
                auths<-head(auths, 6)
                auths
        })
        
        
})
