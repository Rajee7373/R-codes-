
    #### SENTIMENT ANALYSIS ON CLIMATE CHANGE TOPIC READ ONLINE ### 

## Packages required ##
  
library(rJava)
library(tm)		# load all required packages, install if not loaded
library(SnowballC)
library(wordcloud)
library(RWeka)	
library(qdap)		
library(textir)
library(maptpx)
library(data.table)
library(stringr)
library(slam)
library(ggplot2)



makewordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  windows()
  wordcloud(freq.df$word[1:120], freq.df$freq[1:120],scale = c(4,.5),random.order = F, colors=1:10)
} 

# Making positive wordcloud function 
makeposwordc = function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  pos.matches = match(names(freq), c(pos.words,"approvals"))
  pos.matches = !is.na(pos.matches)
  freq_pos <- freq[pos.matches]
  names <- names(freq_pos)
  windows()
  wordcloud(names,freq_pos,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}


words_bar_plot <- function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  library(ggplot2)
  windows()
  ggplot(head(freq.df,50), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Words") + ylab("Frequency") +
    ggtitle("Most frequent words")
  
}

pos_words_bar_plot <- function(x){
  pos.matches = match(colnames(x), pos.words)
  pos.matches = !is.na(pos.matches)
  pos_words_freq = as.data.frame(apply(x, 2, sum)[pos.matches])
  colnames(pos_words_freq)<-"freq"
  pos_words_freq["word"] <- rownames(pos_words_freq)
  # Sorting the words in deceasing order of their frequency
  pos_words_freq <- pos_words_freq[order(pos_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(pos_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Positive words") + ylab("Frequency") +
    ggtitle("Most frequent positive words")
}
neg_words_bar_plot <- function(x){
  neg.matches = match(colnames(x), neg.words)
  neg.matches = !is.na(neg.matches)
  neg_words_freq = as.data.frame(apply(x, 2, sum)[neg.matches])
  colnames(neg_words_freq)<-"freq"
  neg_words_freq["word"] <- rownames(neg_words_freq)
  # Sorting the words in deceasing order of their frequency
  neg_words_freq <- neg_words_freq[order(neg_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(neg_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("words") + ylab("Frequency") +
    ggtitle("Most frequent negative words")
}


# lOADING +VE AND -VE words  
pos.words=scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words=scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words=c(pos.words,"wow", "kudos", "hurray") 			# own positive words to the existing list
neg.words = c(neg.words)
stopwdrds = readLines(file.choose())



  ### Read the text file ###
text <- readLines(file.choose())

  ### Load the data as a corpus##
TextDoc <- Corpus(VectorSource(text))


### Cleaning the text ###
 ## Here the transformation of the text data is done by removing
#the special characters,converting into lower case ,
#stop words are removed and Stemming is done #

#Replacing "/", "@" and "|" with space#
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Converts the text to lower case#
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Removes numbers#
TextDoc <- tm_map(TextDoc, removeNumbers)
# Removes english common stopwords#
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# specifying  your custom stopwords as a character vector#
TextDoc <- tm_map(TextDoc, removeWords, c("well", "also")) 
# Removes punctuations#
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminates extra white spaces#
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)


       ###Building the term document matrix##
## To identify popular or trending topiCS##

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
tdm0 <- as.matrix(TextDoc_dtm)

# Sort by descreasing value of frequency
dtm_v <- sort(rowSums(tdm0),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)

# Display the top 5 most frequent words
head(dtm_d, 5)

# Plotting  the most frequent words to visualize frequent words ##
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="violet", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

 ###From plot it can seen the climate is most frequently used ##


# Term document matrix with inverse frequency 
tdm1 <- TermDocumentMatrix(TextDoc,control = list(weighting = function(p) weightTfIdf(p,normalize = T)))#,stemming=T))
a0 <- NULL
a1 <- NULL
# getting the indexes of documents having count of words = 0
for (i1 in 1:ncol(tdm0))
{ if (sum(tdm0[, i1]) == 0) {a0 = c(a0, i1)} }
for (i1 in 1:ncol(tdm1))
{ if (sum(tdm1[, i1]) == 0) {a1 = c(a1, i1)} }

# Removing empty docs 
tdm0 <- tdm0[,-a0]
tdm1 <- tdm1[,-a1]

# Document term matrix 
dtm0 <- t(tdm0)
dtm1 <- t(tdm1)


   ### Word cloud - TFIDF - Unigram###
makewordc(tdm0)
title(sub = "UNIGRAM - Wordcloud using TF")
## Word cloud clearly the shows the main topic that is climate change and 
# global warming 

# Frequency Barplot - TFIDF - Unigram
words_bar_plot(tdm1)

# Positive word cloud - TF - Unigram$$$
makeposwordc(tdm0)
title(sub = "UNIGRAM - POSITIVE Wordcloud using TFIDF")

# Frequency Barplot - Positive words - Unigram
pos_words_bar_plot(dtm0)

# Frequency Barplot - Negative words - TFIDF
neg_words_bar_plot(dtm1)


# Bi gram word clouds
library(quanteda)
library(Matrix)

    # Bi gram document term frequency 
dtm0_2 <- dfm(unlist(TextDoc),ngrams=3,verbose = F)
tdm0_2 <- t(dtm0_2)
a0 = NULL
for (i1 in 1:ncol(tdm0_2)){ if (sum(tdm0_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		# no. of empty docs in the corpus
if (length(a0) >0) { tdm0_2 = tdm0_2[, -a0]} else {tdm0_2 = tdm0_2};	dim(tdm0_2)	# under TF weighing
a0 <- NULL;i1 <- NULL

dtm0_2 <- t(tdm0_2)

# Bi gram word cloud
makewordc(tdm0_2) # We have too see warnings to edit few words
title(sub = "BIGRAM - Wordcloud using TF")

# Bi gram barplot on TF
words_bar_plot(tdm0_2)

## Bi gram on TFIDF

dtm1_2 <- tfidf(dtm0_2)
tdm1_2 <- t(dtm1_2)
a0 = NULL
for (i1 in 1:ncol(tdm1_2)){ if (sum(tdm1_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		# no. of empty docs in the corpus
if (length(a0) >0) { tdm1_2 = tdm1_2[, -a0]} else {tdm1_2 = tdm1_2};	dim(tdm1_2)	# under TF weighing
a0 <- NULL;i1 <- NULL
dtm1_2 <- t(tdm1_2)

# Bi gram word cloud for TFIDF
makewordc(tdm1_2) # We have too see warnings to edit few words
title(sub = "BIGRAM - Wordcloud using TFIDF")

# Bigram barplot on TFIDF
words_bar_plot(tdm1_2)


# Finding  associations for words that occur at least 50 times###
findAssocs(TextDoc_dtm, terms = c("climate","carbon","global"), corlimit = 0.25)			

# Find associations for words that occur at least 50 times
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50),
           corlimit = 0.25)


    ##Sentiment score using get_sentiment() function ###

syuzhet_vector <- get_sentiment(text, method="syuzhet")
  
head(syuzhet_vector)

summary(syuzhet_vector)

# bing
bing_vector <- get_sentiment(text, method="bing")
head(bing_vector)
summary(bing_vector)

#affin
afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

## Since all the three method used have different scales we use 
#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)


      ## nrc Sentiment Analysis ##
# nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row

d<-get_nrc_sentiment(text)

head (d,10)  ## Top 10 
 ## Three occurences of words associated fear and trust , Four occurences of words 
#associated with positive emotions.

  #transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:253]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Measures on Climate change Sentiments")

#Plot two - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)

                    ######$$$$#####
#Here,In Sentiment analysis involves data cleaning and transformations.
#It is  demonstrated how to create a word frequency table and plot a 
#word cloud along with positive word cloud as well as 
##positive and negative word bar charts, to identify prominent themes
#occurring in the text.Various Wordclouds and bars are used for visualisation.
#Word association analysis using correlation, helped gain context around the 
#prominent themes. It explored four methods to generate Sentiment scores, 
#which proved useful in assigning a numeric value to strength
#(of positivity or negativity) of sentiments in the text and
#allowed interpreting that the average sentiment through the text
#is trending positive. Lastly, it is demonstrated how to implement an
#emotion classification with NRC sentiment and created two plots to 
#analyze and interpret emotions found in the text.

                 ########$$$$#####









                                                                                                                                                 