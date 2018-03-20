#libraries
library(DMwR)
library(RWeka)

#function to create a dataset using sampling with replacement
createBag <- function(train_data){
		bag <- train_data[sample(1:nrow(train_data), replace = TRUE),]
}

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
