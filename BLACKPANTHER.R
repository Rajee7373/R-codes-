library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

      #### IMDB Reviews #####

aurl <- "https://www.imdb.com/title/tt1825683/reviews?ref_=tt_ov_rt"
Bpanther_reviews <- NULL
for (i in 1:25){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  Bpanther_reviews <- c(Bpanther_reviews,rev)
}

length(Bpanther_reviews)

write.table(Bpanther_reviews,"BlackPanther.txt",row.names = F)
getwd()

BPanther <- read.delim('BlackPanther.txt')
str(BPanther)
View(BPanther)


       ### Buidling Corpus and DTM/TDM###
library(tm)
corpus <- BPanther[-1,]
head(corpus)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])


      ### Cleaning  the text ####
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])

corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])

cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset<-tm_map(cleanset,removeWords, c('can','film'))

cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))

cleanset <- tm_map(cleanset, gsub,pattern = 'characterss', replacement = 'character')
# the barplot pulls both character and characters as separate words. this should be 
# counted as one as both holds the same synonym.
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

     ###Term Document Matrix ###
   ## Convert the unstructured data to structured data ##
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

      ### Bar Plots ###

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(25))



     ### Word Cloud ###

library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(1234)
wordcloud(words = names(w), freq = w, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.55, 
          colors=brewer.pal(8, "Dark2"),random.color = T)


     ## Sentiment Score of BlacK Panther movie##
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

   
  ### Reading the File### 

Review_text <- readLines(file.choose())#IMDB Black Panther reviews

s_v <- get_sentences(Review_text)
class(s_v)
str(s_v)

sentiment_vector <- get_sentiment(s_v, method = "bing")
head(sentiment_vector)
afinn_s_v <- get_sentiment(s_v, method = "afinn")

nrc_vector <- get_sentiment(s_v, method="nrc")
head(nrc_vector)

sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)
plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")

## To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative

# and to extract the most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive

poa_v <- my_example_text
poa_sent <- get_sentiment(poa_v, method="bing")
plot(
  poa_sent, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)
# percentage based figures
percent_vals <- get_percentage_values(poa_sent)


plot(
  percent_vals, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)  
ft_values <- get_transformed_values(
  poa_sent, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# categorize each sentence by eight emotions
nrc_data <- get_nrc_sentiment(s_v)
nrc_score_sent <- get_nrc_sentiment(negative)
nrc_score_word <- get_nrc_sentiment('grim')
# subset

sad_items <- which(nrc_data$sadness > 0)
head(s_v[sad_items])

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:10]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)

  ###############@@@@@@@@@@@@@@@@@####################

