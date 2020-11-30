library("twitteR")
library("ROAuth")
library(RCurl)
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
library("devtools")
library("httr")
cred <- OAuthFactory$new(consumerKey='7vEM8lAgPCEekKiXWN8Sxjcae', # Consumer Key (API Key)
                         consumerSecret='PbtP6dRgRCn42oG19jRFZ6BXASNEdrelSSPwxcsG9wuNcT3oiS', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")

library('base64enc')

library('openssl')

library('httpuv')

#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")#
load("twitter authentication.Rdata")
setup_twitter_oauth("7vEM8lAgPCEekKiXWN8Sxjcae", # Consumer Key (API Key)
                    "PbtP6dRgRCn42oG19jRFZ6BXASNEdrelSSPwxcsG9wuNcT3oiS", #Consumer Secret (API Secret)
                    "1284152501768339461-oPg5Dd6RrJQutCAAkPPyZJxtbYfLOb",  # Access Token
                    "ApNTa9wxTeepscvcvjpp7t4clClgt5ZW62RDepFAe6NED")  #Access Token Secret



tweets <- userTimeline('PMO INDIA',n = 1000,includeRts=T)

TweetsDF <- twListToDF(tweets)

write.csv(TweetsDF, "Tweetsnew.csv",row.names = FALSE)


           ###  Sentiment Analysis ###

PMO <- read.csv(file.choose())
str(PMO)

   ### Building Corpus and DTM/TDM###
corpus <- PMO$text
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])


   ### Cleaning the text ###
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
 
cleanset<-tm_map(cleanset,removeWords, c('PMO','can','also','will','many','says','via','shri','minister','unarendramodiu'))
inspect(cleanset[1:5])


        ###Simple Corpus##

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])


            ####Term Document Matrix ###
      ### Convert the unstructured data to structured data###
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 30) # Pull words that were used more than 30 times.
barplot(w, las = 2, col = rainbow(50))



            ###Word Cloud ###

w <- sort(rowSums(tdm), decreasing = TRUE) 
set.seed(777)
wordcloud(words = names(w), freq = w, 
          max.words = 500,random.order = F,
          min.freq =1, 
          colors = brewer.pal(8, 'Dark2'),
          rot.per = 0.6)



  ### Sentiment score for PMO Tweets ####

library(tm)
library(wordcloud)
library(syuzhet)


       #### Reading the  File ###
PMOdata <- read.csv(file.choose(), header = TRUE)
tweets <- as.character(PMOdata$text)
class(tweets)

    ### Obtaining  Sentiment scores ##
s <- get_nrc_sentiment(tweets)

head(s)

tweets[4]

get_nrc_sentiment('nation')
 ##oppurtunities has one value for trust##

get_nrc_sentiment('trade')
 ##Trade has one value for trust ##

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for PMO Tweets')

    ############$$$$$$$$$$$$$$$##############

