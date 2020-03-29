euclideanDist <- function(a, b){
  d = 0
  for(i in c(1:(length(a)) ))
  {
    d = d + (a[[i]]-b[[i]])^2
  }
  d = sqrt(d)
  return(d)
}

knn_predict2 <- function(test_data, train_data, k_value, labelcol){
  pred <- c()  #empty pred vector 
  #LOOP-1
  for(i in c(1:nrow(test_data))){   #looping over each record of test data
    eu_dist =c()          #eu_dist & eu_char empty  vector
    eu_char = c()
    good = 0              #good & bad variable initialization with 0 value
    bad = 0
    
    #LOOP-2-looping over train data 
    for(j in c(1:nrow(train_data))){
 
      #adding euclidean distance b/w test data point and train data to eu_dist vector
      eu_dist <- c(eu_dist, euclideanDist(test_data[i,-c(labelcol)], train_data[j,-c(labelcol)]))
 
      #adding class variable of training data in eu_char
      eu_char <- c(eu_char, as.character(train_data[j,][[labelcol]]))
    }
    
    eu <- data.frame(eu_char, eu_dist) #eu dataframe created with eu_char & eu_dist columns
 
    eu <- eu[order(eu$eu_dist),]       #sorting eu dataframe to gettop K neighbors
    eu <- eu[1:k_value,]               #eu dataframe with top K neighbors
 
    tbl.sm.df<-table(eu$eu_char)
    cl_label<-  names(tbl.sm.df)[[as.integer(which.max(tbl.sm.df))]]
    
    pred <- c(pred, cl_label)
    }
    return(pred) #return pred vector
  }
  

accuracy <- function(test_data,labelcol,predcol){
  correct = 0
  for(i in c(1:nrow(test_data))){
    if(test_data[i,labelcol] == test_data[i,predcol]){ 
      correct = correct+1
    }
  }
  accu = (correct/nrow(test_data)) * 100  
  return(accu)
}

#load data
library(tidyverse)
knn.data <- read.csv("icu.csv") #load icu data
knn.data <- mutate(knn.data, COMA=ifelse(LOC==2, 1, 0)) # create coma
knn.data$STA <- factor(knn.data$STA) # change numerical class to factor
features <- c('TYP', 'AGE', 'INF', 'COMA', 'STA') # select features as instructed
knn.df <- knn.data[features]
labelcol <- 5 # for iris it is the fifth col 
predictioncol<-labelcol+1
# create train/test partitions
set.seed(2)
n<-nrow(knn.df)
knn.df<- knn.df[sample(n),]

train.df <- knn.df[1:as.integer(0.7*n),]

K = 3 # number of neighbors to determine the class
table(train.df[,labelcol])
test.df <- knn.df[as.integer(0.7*n +1):n,]
table(test.df[,labelcol])

predictions <- knn_predict2(test.df, train.df, K,labelcol) #calling knn_predict()

test.df[,predictioncol] <- predictions #Adding predictions in test data as 7th column
print(accuracy(test.df,labelcol,predictioncol))
table(test.df[[predictioncol]],test.df[[labelcol]])
accuracy_score_3 <- accuracy(test.df,labelcol,predictioncol) # create accuracy score object for further plotting

K = 5 # number of neighbors to determine the class
table(train.df[,labelcol])
test.df <- knn.df[as.integer(0.7*n +1):n,]
table(test.df[,labelcol])

predictions <- knn_predict2(test.df, train.df, K,labelcol) #calling knn_predict()

test.df[,predictioncol] <- predictions #Adding predictions in test data as 7th column
print(accuracy(test.df,labelcol,predictioncol))
table(test.df[[predictioncol]],test.df[[labelcol]])
accuracy_score_5 <- accuracy(test.df,labelcol,predictioncol) # create accuracy score object for further plotting

K = 7 # number of neighbors to determine the class
table(train.df[,labelcol])
test.df <- knn.df[as.integer(0.7*n +1):n,]
table(test.df[,labelcol])

predictions <- knn_predict2(test.df, train.df, K,labelcol) #calling knn_predict()

test.df[,predictioncol] <- predictions #Adding predictions in test data as 7th column
print(accuracy(test.df,labelcol,predictioncol))
table(test.df[[predictioncol]],test.df[[labelcol]])
accuracy_score_7 <- accuracy(test.df,labelcol,predictioncol) # create accuracy score object for further plotting


K = 15 # number of neighbors to determine the class
table(train.df[,labelcol])
test.df <- knn.df[as.integer(0.7*n +1):n,]
table(test.df[,labelcol])

predictions <- knn_predict2(test.df, train.df, K,labelcol) #calling knn_predict()

test.df[,predictioncol] <- predictions #Adding predictions in test data as 7th column
print(accuracy(test.df,labelcol,predictioncol))
table(test.df[[predictioncol]],test.df[[labelcol]])
accuracy_score_15 <- accuracy(test.df,labelcol,predictioncol) # create accuracy score object for further plotting

K = 25 # number of neighbors to determine the class
table(train.df[,labelcol])
test.df <- knn.df[as.integer(0.7*n +1):n,]
table(test.df[,labelcol])

predictions <- knn_predict2(test.df, train.df, K,labelcol) #calling knn_predict()

test.df[,predictioncol] <- predictions #Adding predictions in test data as 7th column
print(accuracy(test.df,labelcol,predictioncol))
table(test.df[[predictioncol]],test.df[[labelcol]])
accuracy_score_25 <- accuracy(test.df,labelcol,predictioncol) # create accuracy score object for further plotting

K = 50 # number of neighbors to determine the class
table(train.df[,labelcol])
test.df <- knn.df[as.integer(0.7*n +1):n,]
table(test.df[,labelcol])

predictions <- knn_predict2(test.df, train.df, K,labelcol) #calling knn_predict()

test.df[,predictioncol] <- predictions #Adding predictions in test data as 7th column
print(accuracy(test.df,labelcol,predictioncol))
table(test.df[[predictioncol]],test.df[[labelcol]])
accuracy_score_50 <- accuracy(test.df,labelcol,predictioncol) # create accuracy score object for further plotting

accuracy_score <- c(accuracy_score_3, accuracy_score_5, accuracy_score_7, accuracy_score_15, accuracy_score_25, accuracy_score_50) #plot accuracy vs K
k_numbers <- c(3,5,7,15,25,50)
plot(k_numbers, accuracy_score)
lines(k_numbers, accuracy_score)
