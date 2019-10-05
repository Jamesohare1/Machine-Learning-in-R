#Name: James O'Hare
#Student Number: C02015242
#Machine Learning Assignment part2 - April 2018
#Implementation of bagging and boosting algorithm


#Step 0.1 Preliminary -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#load the data
spam <- read.csv('C:/Users/James/Documents/R/data/spam/spambase.csv', stringsAsFactors = F)

#import libraries
library(DMwR)
library(RWeka)

#function to create a dataset using sampling with replacement
createBag <- function(train_data){
	bag <- train_data[sample(1:nrow(train_data), replace = TRUE),]
	return(bag)
}


#Step 0.2 Algoritms
#function to implement bagging algorithm (decision trees)
bagging_trees <- function(train_data, numbags, classifier){
  classifiers <- list()
  for(j in 1:numbags){
    #create a new bag
    train <- createBag(train_data)
    #print(train.j)
    
    #create a vector of the training labels
    train_labels <- train[,ncol(train_data)]
    #print(train.j_labels)
    
    #remove target from training set
    train_minus_target <- train[,-ncol(train_data)]
    #print(train.j_minus_target)
    
    #create a decision tree
    model <- classifier(as.factor(train_labels)~.,train_minus_target)
    
    #add tree to model
    classifiers[[j]] <- model
  }
  #return the aggreagated lost of decision trees
  return(classifiers)
}  

#function to predict from bagging algorithm (decision trees)
bagging_predict_trees <- function(model, test_data){
  predictions <- data.frame(0)
  for(classifier in model){
    pred<- as.numeric((predict(classifier, test_data)))
    predictions <- cbind(predictions, pred)
  }
  predictions <- predictions[,-1]
  predictions <- replace(predictions, predictions ==1, 0)
  predictions <- replace(predictions, predictions ==2, 1)
  for(row in predictions){
    predictions$Avg <- rowSums(predictions)/ncol(predictions)
  }
  predictions$Avg <- replace(predictions$Avg, predictions$Avg >=0.5, 1)
  predictions$Avg <- replace(predictions$Avg, predictions$Avg <0.5, 0)
  return(predictions$Avg)
}


#function implementing adaboost algorithm (decision trees)
boosting_trees <- function(train_data, iterations, classifier){
  d <- matrix(1/nrow(train_data),nrow(train_data)) 
  alphas <- c()
  #weak_learners <- list()
  model <- list()
  model[[1]] <- list()
  for(j in 1:iterations){
    
    #create test set
    test <- train_data
    
    #create a vector of test labels
    test_labels <- as.vector(test[,ncol(train_data)])
    test_labels <- replace(train_labels, train_labels ==0, -1)
    
    #remove target from set set
    test_minus_target <- test[,-ncol(train_data)]
    
    #sample the data using the instance probailities of the current iteration
    train <- train_data[sample(1:nrow(train_data), replace = TRUE, prob = d),]
    
    #create a vector of the training labels
    train_labels <- as.vector(train[,ncol(train_data)])
    train_labels <- replace(train_labels, train_labels ==0, -1)
    
    #remove target from training
    train_minus_target <- train[,-ncol(spam_train)]
    
    #create a decision tree
    weak_learner <- classifier(as.factor(train_labels)~., train_minus_target)
    
    #predict the outcomes for the test set
    model_pred <- as.numeric(predict(weak_learner, test_minus_target))
    
    #convert factor outputs to numeric and change 1,2s to -1s and 1s
    model_pred <- replace(model_pred, model_pred ==1, -1)
    model_pred <- replace(model_pred, model_pred ==2, 1)
    
    #calculate the model error
    error = 0
    for(i in 1:nrow(d)){
      if(model_pred[i] != test_labels[i]){
        error <- error + d[i]
      }
    }
    
    #calcualte the model alpha
    alpha <- 1/2*log((1-error)/error)
    
    #add alpha to alphas
    alphas[j] <- alpha 
    
    
    #add model to list of models
    model[[1]][[j]] <- weak_learner
    
    #update instance weights
    for(i in 1:nrow(d)){
      d[i] <- d[i] *
        if(model_pred[i] == test_labels[i]) exp(-alpha) else exp(alpha) 
    }
    d <- d/sum(d)
  }
  #model[[1]] <- weak_learners
  model[[2]] <- alphas
  return(model)
}

#function to predict using adaboost algorithm (decision trees)
boosting_trees_predict <- function(model, test_data){
  
  predictions <- data.frame(0)
  iterations <- length(model[[1]])
  for(i in 1:iterations){
    pred <- as.numeric((predict(model[[1]][[i]], test_data)))
    predictions <- cbind(predictions, pred)
  }
  predictions <- predictions[,-1]
  predictions <- replace(predictions, predictions ==1, -1)
  predictions <- replace(predictions, predictions ==2, 1)
  for(row in 1:nrow(predictions)){
    predictions[row, iterations+1] <- sum(predictions[row,1:iterations]*model[[2]])
  }
  predictions[,iterations+1] <- replace(predictions[,iterations+1], predictions[,iterations+1] >=0, 1)
  predictions[,iterations+1] <- replace(predictions[,iterations+1], predictions[,iterations+1] <0, 0)
  return(predictions[,iterations+1])
}


#function to implement bagging (logistic regressions)
bagging_logistic <- function(train_data, numbags, classifier){
  classifiers <- list()
  for(j in 1:numbags){
    #create a new bag
    train <- createBag(train_data)
    
    #create a logistic regression
    model <- classifier(train)
    
    #add tree to model
    classifiers[[j]] <- model
  }
  #return the aggreagated lost of logistic regression models
  return(classifiers)
}  

#function to predict from bagging algorithm (logistic regressions)
bagging_predict_logistic <- function(model, test_data){
  predictions <- data.frame(0)
  for(classifier in model){
    pred<- predict(classifier, test_data)
    predictions <- cbind(predictions, pred)
  }
  predictions <- predictions[,-1]
  predictions <- replace(predictions, predictions >=0, 1)
  predictions <- replace(predictions, predictions <0, -1)
  for(row in predictions){
    predictions$Avg <- rowSums(predictions)/ncol(predictions)
  }
  predictions$Avg <- replace(predictions$Avg, predictions$Avg >=0, 1)
  predictions$Avg <- replace(predictions$Avg, predictions$Avg <0, 0)
  return(predictions$Avg)
}


#function to implement adaboost algorithm (logistic regressions)
boosting_log <- function(train_data, iterations, classifier){
  d <- matrix(1/nrow(train_data),nrow(train_data)) 
  alphas <- c()
  model <- list()
  model[[1]] <- list()
  for(j in 1:iterations){
    
    #create test set
    test <- train_data
    
    #create a vector of test labels
    test_labels <- as.vector(test[,ncol(train_data)])
    test_labels <- replace(train_labels, train_labels ==0, -1)
    
    #remove target from set set
    test_minus_target <- test[,-ncol(train_data)]
    
    #sample the data using the instance probailities of the current iteration
    train <- train_data[sample(1:nrow(train_data), replace = TRUE, prob = d),]
    
    #create a vector of the training labels
    train_labels <- as.vector(train[,ncol(train_data)])
    train_labels <- replace(train_labels, train_labels ==0, -1)
    
    #create a logistic regression 
    weak_learner <- classifier(train)
    
    #predict the outcomes for the test set
    model_pred <- as.numeric(predict(weak_learner, test_minus_target))
    
    #convert outputs to  -1s and 1s
    model_pred <- replace(model_pred, model_pred >=0, 1)
    model_pred <- replace(model_pred, model_pred <0, -1)
    
    error = 0
    for(i in 1:nrow(d)){
      if(model_pred[i] != test_labels[i]){
        error <- error + d[i]
      }
    }
    
    alpha <- 1/2*log((1-error)/error)
    
    #add alpha to alphas
    alphas[j] <- alpha 
    
    #add model to list of models
    model[[1]][[j]] <- weak_learner
    
    #update instance weights
    for(i in 1:nrow(d)){
      d[i] <- d[i] *
        if(model_pred[i] == test_labels[i]) exp(-alpha) else exp(alpha) 
    }
    d <- d/sum(d)
  }
  #model[[1]] <- weak_learners
  model[[2]] <- alphas
  return(model)
}

#function to predict from adaboost algorithm (logistic regression)
boosting_log_predict <- function(model, test_data){
  
  predictions <- data.frame(0)
  iterations <- length(model[[1]])
  for(i in 1:iterations){
    pred <- as.numeric((predict(model[[1]][[i]], test_data)))
    predictions <- cbind(predictions, pred)
  }
  predictions <- predictions[,-1]
  predictions[,iterations] <- replace(predictions[,iterations], predictions[,iterations] >=0, 1)
  predictions[,iterations] <- replace(predictions[,iterations], predictions[,iterations] <0, -1)
  for(row in 1:nrow(predictions)){
    predictions[row, iterations] <- sum(predictions[row,1:iterations]*model[[2]])
  }
  predictions[,iterations] <- replace(predictions[,iterations], predictions[,iterations] >=0, 1)
  predictions[,iterations] <- replace(predictions[,iterations], predictions[,iterations] <0, 0)
  
  return(predictions[,iterations])
}




#Step 1.0 - Prepare the Data for the decision tree-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Randomise the data
spam <- spam[sample(1:nrow(spam)), ]
#Split the data into test and training data
testsize <- floor(nrow(spam)*.8)
spam_train <- spam[1:testsize,]
spam_test <- spam[(testsize+1):nrow(spam),]
#create a vector of the training and test labels
train_labels <- as.vector(spam_train[,ncol(spam_train)])
test_labels <- as.vector(spam_test[,ncol(spam_test)])
#remove target from training and test sets
spam_train_minus_target <- spam_train[,-ncol(spam_train)]
spam_test_minus_target <- spam_test[,-ncol(spam_test)]


#Step 1.1 - run a Decision Tree on the data set-------------------------------------------------------------------------------------------------------------
#Train the model on the training set
spam_dt <- J48(as.factor(train_labels)~., spam_train_minus_target)
#predict the outcomes for the test set
spam_pred_dt <- predict(spam_dt, spam_test_minus_target)
#Eevaluate the model using CrossTable
library('gmodels')
CrossTable(spam_pred_dt, test_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))


#Step 1.2. run Bagging Algorithm with decision tree on the data set----------------------------------------------------------------------------------------------
#Train the model on the training set
bagging_tree_model <- bagging_trees(spam_train, 1000, J48)
#predict the outcomes from the test set
bagging_tree_predictions <- bagging_predict_trees(bagging_tree_model, spam_test_minus_target)
#Eevaluate the model using CrossTable
CrossTable(bagging_tree_predictions,test_labels,  prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))


#Step 1.3. run Boosting Algorithm with decision tree on the data set----------------------------------------------------------------------------------------------
#Train the model on the training set
boosting_tree_model <- boosting_trees(spam_train, 500, J48)
#predict the outcomes from the test set
boosting_tree_predictions <- boosting_trees_predict(boosting_tree_model, spam_test_minus_target)
#Eevaluate the model using CrossTable
CrossTable(boosting_tree_predictions, test_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))


#Step 2.0 - Prepare the Data for the regression-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#normalise the data
library(BBmisc)
spam_normal <- normalize(spam, method = "range", range = c(0, 1), margin = 1L, on.constant = "quiet")
spam_train_normal <- spam_normal[1:testsize,]
spam_test_normal <- spam_normal[(testsize+1):nrow(spam),]


#Step 2.1 - run a logistic regression on the data set----------------------------------------------------------------------------------------------------------------------
#function for creating a logistic regression
logRegress <- function(Inputdata){
  regression <- glm(col_58 ~ col_1 + col_2 +col_3 + col_4 + col_5 + col_6 + col_7 + col_8 + col_9 + col_10 +
                       col_11 + col_12 +col_13 + col_14 + col_15 + col_16 + col_17 + col_18 + col_19 + col_20 +
                       col_21 + col_22 +col_23 + col_24 + col_25 + col_26 + col_27 + col_28 + col_29 + col_30 +
                       col_31 + col_32 +col_33 + col_34 + col_35 + col_36 + col_37 + col_38 + col_39 + col_40 +
                       col_41 + col_42 +col_43 + col_44 + col_45 + col_46 + col_47 + col_48 + col_49 + col_50 +
                       col_51 + col_52 +col_53 + col_54 + col_55 + col_56 + col_57
                     ,  family = binomial, data = Inputdata)
  return(regression)
}

#train the model on the training set
spam_lr <- logRegress(spam_train_normal)
#predict the outcomes on the test set
spam_test_minus_target_normal <-spam_test_normal[,-58]
spam_pred_lr <- predict(spam_lr, spam_test_minus_target_normal)
#convert the output to binary factors
spam_pred_lr <- replace(spam_pred_lr, spam_pred_lr>0, 1)
spam_pred_lr <- replace(spam_pred_lr, spam_pred_lr<0, 0)
#Eevaluate the model using CrossTable
CrossTable(spam_pred_lr, test_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))


#Step 2.2. run Bagging Algorithm with logistic regression on the data set----------------------------------------------------------------------------------------------
#Train the model on the training set
bagging_log_model <- bagging_logistic(spam_train_normal, 1000, logRegress)
#predict the outcomes from the test set
bagging_log_predictions <- bagging_predict_logistic(bagging_log_model, spam_test_minus_target_normal)
#Eevaluate the model using CrossTable
CrossTable(bagging_log_predictions, test_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))



#Step 2.3. run Boosting Algorithm with logistic regression on the data set -----
#Train the model on the training set
boosting_log_model <- boosting_log(spam_train_normal, 1000, logRegress)
#predict the outcomes from the test set
boosting_log_predictions <- boosting_log_predict(boosting_log_model, spam_test_minus_target_normal)
#Eevaluate the model using CrossTable
CrossTable(boosting_log_predictions, test_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))
