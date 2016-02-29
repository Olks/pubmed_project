library(shiny) 
library(RISmed)
library(tm)
library(qdap)

shinyServer(function(input, output) { 
        
        search_topic <- eventReactive(input$searchButton, {input$term})
        
        output$articlesNr <- renderText({
                search_query <- EUtilsSummary(search_topic(), retmax=40) #type="esearch", db="pubmed", datetype='pdat') #mindate=2014, maxdate=2016)
                N <- QueryCount(search_query)
                paste0("There are ", N, " articles referring to <", search_topic(), ">" )
        })
        
        
        output$authorsTable <- renderTable({
                search_query <- EUtilsSummary(search_topic(), retmax=40) #type="esearch", db="pubmed", datetype='pdat') #mindate=2014, maxdate=2016)
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
        
        relevantSort <- eventReactive(input$sortButton, {input$term})
        output$relevancePlot <- renderPlot({
                search_query <- EUtilsSummary(search_topic(), retmax=40) #type="esearch", db="pubmed", datetype='pdat') #mindate=2014, maxdate=2016)
                fetch <- EUtilsGet(search_query)
                
                pubmed_dataMesh <- Mesh(fetch)
                pubmed_dataAb <- AbstractText(fetch)
                pubmed_dataTi <- ArticleTitle(fetch)
                pubmed_dataYear <- YearPubmed(fetch)
                N <- length(pubmed_dataTi)  # number of documents
                
                pubmed_Mesh <- rep("",N)
                for(i in 1:N){
                        if (!is.na(pubmed_dataMesh[[i]])[1]){
                                terms <- as.vector(pubmed_dataMesh[[i]]$Heading)
                                for(j in 1:length(terms)){
                                        pubmed_Mesh[i] <- paste(pubmed_Mesh[i],pubmed_dataMesh[[i]]$Heading[j])
                                }
                                pubmed_Mesh[i] <- strip(pubmed_Mesh[i])
                        }
                }
                
                
                texts.processing <- function(texts.vector){
                        TextsCorpus <- VCorpus(VectorSource(texts.vector), 
                                               readerControl = list(reader = readPlain, language = "en"))
                        TextsCorpus <- tm_map(TextsCorpus, removePunctuation) 
                        TextsCorpus <- tm_map(TextsCorpus, removeNumbers) 
                        TextsCorpus <- tm_map(TextsCorpus, tolower)
                        TextsCorpus <- tm_map(TextsCorpus, removeWords, qdapDictionaries::Top200Words) 
                        TextsCorpus <- tm_map(TextsCorpus, stripWhitespace)   
                        TextsCorpus <- tm_map(TextsCorpus, PlainTextDocument)
                        dtmTexts <- DocumentTermMatrix(TextsCorpus)  
                        dtmTexts <- removeSparseTerms(dtmTexts,0.999)
                        dtmTexts <- as.matrix(dtmTexts)
                        rownames(dtmTexts) <- as.character(1:dim(dtmTexts)[1])
                        dtmTexts <- as.data.frame(dtmTexts)
                        return(dtmTexts)
                }
                
                
                
                dtmMeshTerms <- texts.processing(pubmed_Mesh)
                dtmAbstracts <- texts.processing(pubmed_dataAb)
                dtmTitles <- texts.processing(pubmed_dataTi)
                
                terms <- strsplit(search_topic(), " ")[[1]]
                n.terms <- length(terms)
                
                tables <- list(a=dtmAbstracts,b=dtmTitles,c=dtmMeshTerms)
                IDF <- matrix(0,n.terms,3)    # global weights of searched terms
                for (i in 1:n.terms){
                        for (j in 1:3){
                                if(terms[i] %in% colnames(tables[[j]])){
                                        IDF[i,j] <- log(N/sum(tables[[j]][,terms[i]]!=0))
                                }
                                
                        }
                }
                
                # Abstract, Title, Mesh
                FW <- c(5.0, 16.0, 5.0)
                
                inTerms <- list(terms[terms %in% colnames(dtmAbstracts)],
                                terms[terms %in% colnames(dtmTitles)],
                                terms[terms %in% colnames(dtmMeshTerms)])
                
                # TF
                make.TF <- function(dtmTable){
                        alfa <- 0.0044
                        lambda <- 0.7
                        columnNames <- terms[terms %in% colnames(dtmTable)]
                        N <- dim(dtmTable)[1] #number of documents
                        cn <- length(columnNames) # number of terms (columns)
                        TF <- matrix(0,N,cn)
                        TF <- as.data.frame(TF)
                        colnames(TF) <- terms[terms %in% colnames(dtmTable)]
                        localTerms<-terms[terms %in% colnames(dtmTable)]
                        dls <- apply(dtmTable, 1, sum) 
                        for (j in 1:cn){  # for each term
                                for (i in 1:N){   # for each document
                                        dl<- dls[i]   # document length
                                        if (dl < 250) dl <- 250
                                        lc <- dtmTable[i,localTerms[j]]  # term frequency
                                        TF[i,j] <- 1/(1+exp(alfa*dl)*lambda^(lc-1)) 
                                }
                        }
                        return(TF)
                }
                
                
                
                TFabstracts <- make.TF(dtmAbstracts)
                TFtitles <- make.TF(dtmTitles)
                TFs <- list(a=TFabstracts,b=TFtitles)
                if(dim(dtmMeshTerms)[2]!=0){
                        TFmesh <- make.TF(dtmMeshTerms)
                        TFs <- list(a=TFabstracts,b=TFtitles,c=TFmesh)
                }
                
                
                relevance.index <- function(){
                        indices <- c()
                        Yc <- as.numeric(format(Sys.Date(), "%Y"))  # current year
                        for (i in 1:N) {
                                Yp <- pubmed_dataYear[i]
                                difference <- Yc - Yp
                                if (difference <8){
                                        PW <- 1 + (7-difference)/10        
                                } else {
                                        PW <- 1
                                }
                                indices[i] <- 0 
                                for (j in 1:n.terms){
                                        for (h in 1:3){
                                                allTogether <- 0
                                                if(terms[j] %in% inTerms[[h]]){
                                                        allTogether <- allTogether + 1
                                                        indices[i] <- indices[i] + FW[h]*IDF[j,h]*TFs[[h]][i,j] + allTogether
                                                }
                                        }
                                }
                                indices[i] <- indices[i] * PW
                        }
                        return (indices)
                }
                
                relevances <- relevance.index()
                
                max.rel <- head(sort(relevances, decreasing = TRUE, index.return = TRUE)$ix,20)
                #head(sort(relevances, decreasing = TRUE, index.return = TRUE)$x,20)
                plot(sort(relevances, decreasing = TRUE, index.return = TRUE)$x,pch=19,cex=0.5)
                #pubmed_dataTi[max.rel]
                
        })
        
        
})
