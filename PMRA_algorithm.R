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


numsNonEmpty <- which(pubmed_dataAb!="")
AbstractsNonEmpty <- pubmed_dataAb[numsNonEmpty] # remove empty abstracts
TitlesNonEmpty <- pubmed_dataTi[numsNonEmpty]

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

### DocumentTermMatrix as data frame
dtmAbstracts <- texts.processing(AbstractsNonEmpty)
dim(dtmAbstracts)
### parameters for PMRA model
lambda <- 0.022
mi <- 0.013

omega.index <- function(dtm){
        x <- dim(dtm)[2] # number of terms
        y <- dim(dtm)[1] # number of documents
        omegas <- matrix(0,y,x)
        omega <- function(k,l){
                omega <- ((1+((mi/lambda)^(k-1))*exp(-(mi-lambda)*l))^(-1))*sqrt(1/k)
                return(omega)
        }
        for (i in 1:x){  # for each term
                for (j in 1:y){ # form each document
                        if (dtm[j,i]!=0) {
                                k <- dtm[j,i]   # frequency of the i term in j document 
                                l <- sum(dtm[j,])  # length of document j
                                omegas[j,i] <- omega(k,l)
                        }
                        print(i)
                }
        }
        return(omegas)
}

#### omega coefficients for PMRA algoritm
omega.indices <- omega.index(dtmAbstracts)


####  similarity table
similarity <- function(omegas.table){
        n <- dim(omegas.table)[1]   #number of documents
        similarity.table <- matrix(0,n,n)
        
        for (i in 1:n) {  # for each document
                for (j in 1:n) { # for each another document
                        if (j>i){
                                similarity.table[i,j] <- similarity.table[j,i]
                        } else {
                                for (h in 1:dim(omegas.table)[2]){ # for each term
                                        similarity.table[i,j] <- 
                                                similarity.table[i,j] + 
                                                omegas.table[i,h] * omegas.table[j,h]
                                }
                        }
                        print(i)
                }
        }
        return (similarity.table)
}

#### similarity table
similarities <- similarity(omega.indices)

#### 10 most similar articles for first article 
max.similar <- head(sort(similarities[,1], decreasing=TRUE, index.return=TRUE)$ix,10)
TitlesNonEmpty[max.similar]




