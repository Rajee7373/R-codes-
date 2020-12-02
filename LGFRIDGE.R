         ### Analysis on LG Fridge reviews ### 
      
library("syuzhet")
library(rlang)
library(caret)
library(quanteda)
library(readtext)
library(irlba)
library(wordcloud)
library(irlba)
library(topicmodels)
library(rvest)
library(XML)
library(magrittr)

        ######### Amazon Reviews #########

aurl <-  "https://www.amazon.in/dp/B079QP11T3#customerReviews"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="")))
  rev <- murl %>%
    html_nodes(".a-size-base") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"LGFRIDGE",row.name=F)
getwd()

LG.raw <- tokens(amazon_reviews,what="word",remove_number=T,remove_punct = T,remove_symbols = T,split_hyphens=T)
#Lower case the tokens
LG.raw <- tokens_tolower(LG.raw)
#to remove stopwords
LG.raw <- tokens_select(LG.raw,stopwords(),selection = "remove")
LG.raw <- tokens_wordstem(LG.raw,language = "english")
LG.raw <- tokens_select(LG.raw,c("page_start","asin","b079qp11t","februari","custom_rate",
                                 "septemb","august","october","question"),selection = "remove",padding = T)
LG.raw <- tokens_ngrams(LG.raw,n=1:2)
LG.raw.dfm <- dfm(LG.raw)
LG.raw.matrix <- as.matrix(LG.raw.dfm)
LG.raw.matrix1 <- as.data.frame(LG.raw.matrix)

s <- 0
for (i in 1:nrow(LG.raw.matrix)){
     if(sum(LG.raw.matrix1[i,])==0){
         LG.raw.matrix2 <- LG.raw.matrix1[-i,]
        s <- s+1
       }
    
     }
print(s)


LG.raw.matrix3<- LG.raw.matrix2[which(rowSums(LG.raw.matrix2)>0),]

#function for calculating the term frequency
term.frequency <- function(row){
     row/sum(row)  
     }
#function for calculating inverse document frequency
inverse.doc.freq <- function(col){
       cor.size <- length(col)
       doc.size <- length(which(col>0))
       log10(cor.size/doc.size)
    }  
#fun for calculating TF-IDF
   tf.idf <- function(tf,idf){
       tf*idf
     }
#Normalizing all document via tf##
LG.raw.dfm <- apply(LG.raw.matrix3,1,term.frequency)
dim(LG.raw.dfm)

#Calculating IDF##
LG.raw.idf <- apply(LG.raw.matrix3, 2,inverse.doc.freq)

#Calculating TF-IDF
LG.raw.tfidf <- apply(LG.raw.dfm, 2,tf.idf,idf=LG.raw.idf)
dim(LG.raw.tfidf)
v <- sort(rowSums(LG.raw.tfidf[-1,]),decreasing = T)
d <- data.frame(word=names(v),freq=v)

       #### Generating Word Cloud###
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


   ############# Sentiment Score ######################

LG_text <- readLines(file.choose())#taking our LGFRIDGE file 

s_v <- get_sentences(LG_text)
class(s_v)

nrc_vector <- get_sentiment(s_v, method="nrc")

# categorize each sentence by eight emotions
nrc_data <- get_nrc_sentiment(s_v)

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:10]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Sentiment score", xlab = "Percentage",
        col = 1:8)

### From the plot it can be seen that the Product analysed had more positive 
## reviews.
 

