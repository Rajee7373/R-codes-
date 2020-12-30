               ####Packages required ###

library(tidyverse)
library(Matrix)
library(recommenderlab)
library(kableExtra)
library(gridExtra)
library(caTools)


                  ###Reading the CSV file ##

book_ratings <- read.csv("C:/Users/Admin34/Desktop/book.csv", sep = ",", header = T, stringsAsFactors = F)

View(book_ratings)


# table dimensions
dim(book_ratings)

summary(book_ratings)


# first few ratings for books
head(book_ratings, 10)
object.size(book_ratings)

hist(book_ratings$Book_rating)


  #### Converting data into Matrix#

bmatrix <- as(book_ratings, "realRatingMatrix")
dim(bmatrix@data)

sim <- similarity(bmatrix[1:10, ], method = "cosine", which = "users")
image(as.matrix(sim), main = "User Similarity")

sim2 <- similarity(bmatrix[ ,1:10], method = "cosine", which = "items")
image(as.matrix(sim2), main = "Item Similarity")


## Average book ratings ###

avg_book_ratings <- data.frame("avg_rating" = colMeans(bmatrix)) %>% 
  ggplot(aes(x = avg_rating)) + 
  geom_histogram(color = "black", fill = "lightgreen") + 
  ggtitle("Distribution of Average Ratings for Books")
avg_book_ratings

image(bmatrix[1:100, 1:100], main = "First 100 users and books")
min_readers <- quantile(rowCounts(bmatrix), 0.99)
min_books <- quantile(colCounts(bmatrix), 0.99)
a <- image(bmatrix[rowCounts(bmatrix) > min_readers, colCounts(bmatrix) > min_books], main = "Non-Normalized")

 
# to eliminate bias therefore average rating would be 0
book_norm <- normalize(bmatrix)
b <- image(book_norm[rowCounts(book_norm) > min_readers, colCounts(book_norm) > min_books], main = "Normalized")
grid.arrange(a, b, ncol = 2)


     ###Splitting the data into Train and test data ###

train <- sample(x = c(T, F), size = nrow(bmatrix), replace = T, prob = c(0.8, 0.2)) 
books_train <- bmatrix[train, ] 
books_test <- bmatrix[-train, ]

  
      ###Building the Model on train data - MOdel 1##
 # Model 1##

Book_popularity=Recommender(books_train,method="POPULAR")

   ####Prediction based on above model using test data ###

Booklist=predict(Book_popularity,books_test,n=1)
 
list=as(Booklist,"list")
list
getmode <- function(l) {
  uniqv <- unique(l)
  uniqv[which.max(tabulate(match(l, uniqv)))]
}
getmode(list)

##[1] "In the Beauty of the Lilies" ##

### Similarly -Booklist=predict(Book_popularity,books_test,n=5)##
 
#[1] "In the Beauty of the Lilies"         "Black House"                        
#[3] "White Oleander : A Novel"            "Nowle's Passing: A Novel"           
#[5] "Something Under the Bed Is Drooling"

## n=5 gives the above five books by Popular method ##

##############$$$$$$$$$$$$$#######################





    ######CODE NT WORKING  from here ###


    ### Collaborative filtering method ###

## IBCF based Model using train data ##

Imodel <- Recommender(data = books_train, method = "IBCF")

### Prediction based the above model using test data ##

Ipredict <- predict(Imodel, newdata = books_test, n = 1)

Ipredict




##UBCF##
Umodel <- Recommender(data = books_train, method = "UBCF")
Umodel

Upredict <- predict(Umodel, newdata = books_test, n = 5) %>% list()
Upredict

########UBCF###


     
    