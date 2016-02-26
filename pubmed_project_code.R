library(RISmed)
library(tm)
library(qdap)
library(wordcloud)
library(RColorBrewer)


search_topic <- 'psoriasis arthritis'
search_query <- EUtilsSummary(search_topic, retmax=615)
fetch <- EUtilsGet(search_query)

pubmed_dataAb <- AbstractText(fetch)
pubmed_dataTi <- ArticleTitle(fetch)
pubmed_dataMesh <- Mesh(fetch)

AuthorList<-Author(fetch)
eachAuthor <- function(x){
        authors<-c()
        for (i in 1:dim(x)[1]){
                authors[i]<-paste(x$LastName[i],x$ForeName[i],x$Initials[i])
        }
        return(authors)
}


numsNonEmpty <- which(pubmed_dataAb!="")
AbstractsNonEmpty <- pubmed_dataAb[numsNonEmpty] # remove empty abstracts
TitlesNonEmpty <- pubmed_dataTi[numsNonEmpty]

# titles of citations without Titles
numsEmpty <- which(pubmed_dataAb=="")
titlesOfEmptyAbstract <- pubmed_dataTi[numsEmpty]


#Corpus for Abstracts
AbstractsCorpus <- VCorpus(VectorSource(AbstractsNonEmpty), 
                           readerControl = list(reader = readPlain, language = "en"))
AbstractsCorpus <- tm_map(AbstractsCorpus, removePunctuation) 
AbstractsCorpus <- tm_map(AbstractsCorpus, removeNumbers) 
AbstractsCorpus <- tm_map(AbstractsCorpus, tolower)
AbstractsCorpus <- tm_map(AbstractsCorpus, removeWords, qdapDictionaries::Top200Words) 
AbstractsCorpus <- tm_map(AbstractsCorpus, stripWhitespace)   
AbstractsCorpus <- tm_map(AbstractsCorpus, PlainTextDocument) 

#Corpus for Titles
TitlesCorpus <- VCorpus(VectorSource(TitlesNonEmpty), 
                           readerControl = list(reader = readPlain, language = "en"))
TitlesCorpus <- tm_map(TitlesCorpus, removePunctuation) 
TitlesCorpus <- tm_map(TitlesCorpus, removeNumbers) 
TitlesCorpus <- tm_map(TitlesCorpus, tolower)
TitlesCorpus <- tm_map(TitlesCorpus, removeWords, qdapDictionaries::Top200Words) 
TitlesCorpus <- tm_map(TitlesCorpus, stripWhitespace)   
TitlesCorpus <- tm_map(TitlesCorpus, PlainTextDocument) 


#####   tm processing
dtmAbstracts <- DocumentTermMatrix(AbstractsCorpus)   
dtmTitles <- DocumentTermMatrix(TitlesCorpus) 
dim(dtmAbstracts)
dim(dtmTitles)
dtmAbstracts <- removeSparseTerms(dtmAbstracts,0.6)
dtmTitles <- removeSparseTerms(dtmTitles,0.8)
dtmAbstracts <- as.matrix(dtmAbstracts)
dtmTitles <- as.matrix(dtmTitles)

colnames(dtmAbstracts)
colnames(dtmTitles)

rownames(dtmAbstracts) <- as.character(1:dim(dtmAbstracts)[1])
rownames(dtmTitles) <- as.character(1:dim(dtmTitles)[1])

dtmAbstracts <- as.data.frame(dtmAbstracts)
dtmTitles <- as.data.frame(dtmTitles)

search_terms <- strsplit(search_topic," ")[[1]]

citation.index <- rep(0,dim(dtmAbstracts)[1])

for(i in 1:length(search_terms)){
        occurence.sum.Abstracts <- sum(dtmAbstracts[,search_terms[i]])
        citation.index <- citation.index + dtmAbstracts[,search_terms[i]]/occurence.sum.Abstracts + (dtmTitles[,search_terms[i]]!=0)/20
}


sorted.citation.list <- sort(citation.index,decreasing = TRUE, index.return=TRUE)
head(sorted.citation.list$ix,397)
head(sorted.citation.list$x,20)

######  this is my order of relevance







##### sortowanie autorów
Last<-sapply(AuthorList, eachAuthor)
auths<-as.data.frame(sort(table(unlist(Last)), dec=TRUE))
colnames(auths)<-"Count"
auths <- cbind(Author = rownames(auths), auths)
rownames(auths) <- NULL

auths<-head(auths, 6)
auths
head(pubmed_data,2)
class(pubmed_data[2,2])
pubmed_data[2,2]







###### - change corpus into list of characters
abstr <- list() 
for(i in 1:length(abstarctsStem)){
        abstr[[i]] <- as.character(abstarctsStem[[i]])
} 
remove <- gsub("\\s+"," ", abstr)
terms <- sapply(remove, function(x)strsplit(x," "))
terms <- sapply(terms, function(x)return(x[x!=""]))
######




# Determine number of clusters
mydata <- scale(freqM)
mydata <- freqM
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)


ones <- which(mydata$fit.cluster==1)


library(qdap)
myFunc<-function(argument){
        articles1<-data.frame('Abstract'=AbstractText(fetch), 'Year'=YearPubmed(fetch))
        abstracts1<-articles1[which(articles1$Year==argument),]
        abstracts1<-data.frame(abstracts1)
        abstractsOnly<-as.character(abstracts1$Abstract)
        abstractsOnly<-paste(abstractsOnly, sep="", collapse="")
        abstractsOnly<-as.vector(abstractsOnly)
        abstractsOnly<-strip(abstractsOnly)
        stsp<-rm_stopwords(abstractsOnly, stopwords = qdapDictionaries::Top200Words)
        ord<-as.data.frame(table(stsp))
        ord<-ord[order(ord$Freq, decreasing=TRUE),]
        head(ord,20)
}

myFunc(2016)






# k-means
mydata <- scale(f) # standardize variables
fit <- kmeans(mydata, 5)
summary <- aggregate(f, by=list(fit$cluster),FUN=mean)
mydata <- data.frame(f, fit$cluster)

## chmura s³ów dla grupy 1
dl <- dim(mydata)[2] 
par(mfrow=c(2,2))
wordcloud(words=colnames(mydata)[-dl],freq=as.numeric(apply(mydata[mydata$fit.cluster==1,-dl],2,sum))*1000, max.words = 40)
wordcloud(words=colnames(mydata)[-dl],freq=as.numeric(apply(mydata[mydata$fit.cluster==2,-dl],2,sum))*1000, max.words = 40)
wordcloud(words=colnames(mydata)[-dl],freq=as.numeric(apply(mydata[mydata$fit.cluster==3,-dl],2,sum))*1000, max.words = 40)
wordcloud(words=colnames(mydata)[-dl],freq=as.numeric(apply(mydata[mydata$fit.cluster==4,-dl],2,sum))*1000, max.words = 40)

#### Mozna dodac jacy autorzy s¹ w ka¿dej z grup

### most frequent words in all abstracts
dtm <- DocumentTermMatrix(abstarctsStem)
dtmSparse <- removeSparseTerms(dtm,0.2)
freqM <- as.matrix(dtmSparse)
colnames(freqM)
frequency <- colSums(freqM)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency, 30)
tail(frequency, 30)


##########
# freqM zawiera liczebnoœci bezwzglêdne, wiêc podzlê je prze liczbê s³ów w artykule
sums <- apply(freqM, 1, sum)
# f - tablica w czêstoœciami wzglêdnymi
freqM <- freqM/sums
rownames(freqM) <- as.character(1:dim(freqM)[1])
freqM <- as.data.frame(freqM)
# in which abstract searching words are the most often 'psoriasis arthritis'
which.max(freqM$psoriasis)
which.max(freqM$arthritis)
head(sort(freqM$arthritis,decreasing=T))
head(sort(freqM$psoriasis,decreasing=T))
freqM$arthritis[614]
freqM$psoriasis[614]



#### checking for words that occure often in most of abstracts
####
# freqM - my matrix
dtm <- DocumentTermMatrix(abstarctsStem)   
dim(dtm)   
dtmSparse <- removeSparseTerms(dtm,0.99)
freqM <- as.matrix(dtmSparse)
rownames(freqM) <- as.character(1:dim(freqM)[1])
names <-colnames(freqM)
n.terms<-length(names)
##

# freqM zawiera liczebnoœci bezwzglêdne, wiêc podzlê je prze liczbê s³ów w artykule
sums <- apply(freqM, 1, sum)
# f - tablica w czêstoœciami wzglêdnymi
freqM <- freqM/sums


terms.occurence <- function(x){
        terms.occ <- c()
        f <- 0.01  #minimum frequency of occurence in a given abstract
        for (i in 1:n.terms){
                terms.occ[i] <- sum(x[,i]>0.01) 
                }
        return(terms.occ)
        }


freqM.terms.occurence <- terms.occurence(freqM)



head(sort(freqM.terms.occurence, decreasing=T, index.return=T)$x,20)

max.index <- head(sort(freqM.terms.occurence, decreasing=T, index.return=T)$ix,100)
colnames(freqM)[max.index]


freqM <- freqM[,-max.index]
freqM[,"arthritis"][1:10]
n.terms
dim(freqM)

############# k-means
mydata <- scale(freqM)
mydata <- freqM
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:7) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# K-Means Cluster Analysis
fit <- kmeans(mydata, 6) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)



TitleList<-ArticleTitle(fetch)
Titles <- TitleList[which(numsNonEmpty==TRUE)]

ones <- which(mydata$fit.cluster==1)
Titles[ones]

twos <- which(mydata$fit.cluster==2)
Titles[twos]

three <- which(mydata$fit.cluster==3)
Titles[three]

four <- which(mydata$fit.cluster==4)
Titles[four]

five <- which(mydata$fit.cluster==5)
Titles[five]

six <- which(mydata$fit.cluster==6)
Titles[six]

length(ones)
length(twos)
length(three)
length(four)
length(five)
length(six)

library(wordcloud)
library(RColorBrewer)
dl <- dim(mydata)[2] 
par(mfrow=c(2,3))
wordcloud(words=colnames(mydata)[-dl],freq=as.numeric(apply(mydata[mydata$fit.cluster==1,-dl],2,sum))*1000, max.words = 40)
wordcloud(words=colnames(mydata)[-dl],freq=as.numeric(apply(mydata[mydata$fit.cluster==2,-dl],2,sum))*1000, max.words = 40)
wordcloud(words=colnames(mydata)[-dl],freq=as.numeric(apply(mydata[mydata$fit.cluster==3,-dl],2,sum))*1000, max.words = 40)
wordcloud(words=colnames(mydata)[-dl],freq=as.numeric(apply(mydata[mydata$fit.cluster==4,-dl],2,sum))*1000, max.words = 40)
wordcloud(words=colnames(mydata)[-dl],freq=as.numeric(apply(mydata[mydata$fit.cluster==5,-dl],2,sum))*1000, max.words = 40)
wordcloud(words=colnames(mydata)[-dl],freq=as.numeric(apply(mydata[mydata$fit.cluster==6,-dl],2,sum))*1000, max.words = 40)


