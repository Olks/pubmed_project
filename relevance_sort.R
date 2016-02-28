### relevance sort

library(RISmed)
library(tm)
library(qdap)
library(wordcloud)
library(RColorBrewer)


search_topic <- 'plaque psoriasis'
search_query <- EUtilsSummary(search_topic, retmax=4000) #mindate=2014, maxdate=2016)
fetch <- EUtilsGet(search_query)

pubmed_dataMesh <- Mesh(fetch)
sum(is.na(pubmed_dataMesh))
pubmed_dataAb <- AbstractText(fetch)
pubmed_dataTi <- ArticleTitle(fetch)
pubmed_dataYear <- YearPubmed(fetch)
sum(is.na(pubmed_dataYear))

N <- length(pubmed_dataTi)  # number of documents

pubmed_Mesh <- rep("",N)
for(i in 1:N){
        if (!is.na(pubmed_dataMesh[[i]])[1]){
                terms <- as.vector(pubmed_dataMesh[[i]]$Heading)
                for(j in 1:length(terms)){
                        pubmed_Mesh[i] <- paste(pubmed_Mesh[i],pubmed_dataMesh[[i]]$Heading[j])
                        #print(i)
                }
                pubmed_Mesh[i] <- strip(pubmed_Mesh[i])
        }
}

dtmMeshTerms <- texts.processing(pubmed_Mesh)
dtmAbstracts <- texts.processing(pubmed_dataAb)
dtmTitles <- texts.processing(pubmed_dataTi)

terms <- strsplit(search_topic, " ")[[1]]
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
FW <- c(5.0, 16.0, 10.0)

# TF
make.TF <- function(dtmTable){
        alfa <- 0.0044
        lambda <- 0.7
        TF <- data.frame()
        columnNames <- c()
        N <- dim(dtmTable)[1] #number of documents
        dls <- apply(dtmTable, 1, sum) 
        for (j in 1:n.terms){
                if(terms[j] %in% colnames(dtmTable)){
                        for (i in 1:N){   # for each document
                                dl<- dls[i]   # document length
                                if (dl < 250) dl <- 250
                                lc <- dtmTable[i,terms[j]]  # term frequency
                                TF[i,j] <- 1/(1+exp(alfa*dl)*lambda^(lc-1)) 
                        #print(i)
                        }
                }
        }
        colnames(TF) <- terms[terms %in% colnames(dtmTable)]
        return(TF)
        
}

TFabstracts <- make.TF(dtmAbstracts)
TFtitles <- make.TF(dtmTitles)
TFmesh <- make.TF(dtmMeshTerms)
TFs <- list(a=TFabstracts,b=TFtitles,c=TFmesh)

inTerms <- list(terms[terms %in% colnames(dtmAbstracts)],
                terms[terms %in% colnames(dtmTitles)],
                terms[terms %in% colnames(dtmMeshTerms)])


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
head(sort(relevances, decreasing = TRUE, index.return = TRUE)$x,20)
pubmed_dataTi[max.rel]
plot(sort(relevances, decreasing = TRUE, index.return = TRUE)$x,pch=19,cex=0.5)
