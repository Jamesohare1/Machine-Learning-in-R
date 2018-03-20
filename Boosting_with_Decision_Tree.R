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

