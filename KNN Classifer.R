#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# MODELS (k nearest neighbour)

#Model 1
#Used when smaller metric implies more similarity
#return index of k nearest neighbours
findKNearest <- function(k, vec, dataFrame, metric){
		nearest = c()
		eucDist = c(distapply(vec, dataFrame, metric))
		for(i in 1:k){
			nearest[i] = which.min(eucDist)
			eucDist[which.min(eucDist)] = 1000000000000
			}
		return(nearest)
}

KNRecommendation <- function (k, vec, dataFrame1, dataFrame, metric){
	indices = findKNearest(k, vec, dataFrame, metric)
	result = c()
	for(i in 1:k){
		result[i] = dataFrame1[indices[i],ncol(dataFrame1)]
	}
	return(result)
}




#Model 2
#Used when larger metric implies more similarity
#Similarity Model
#return index of k nearest neighbours
findKNearestSimilarity <- function(k, vec, dataFrame, metric){
		nearest = c()
		eucDist = c(distapply(vec, dataFrame, metric))
		for(i in 1:k){
			nearest[i] = which.max(eucDist)
			eucDist[which.max(eucDist)] = 0
			}
		return(nearest)
}

KNRecommendationSimilarity <- function (k, vec, dataFrame1, dataFrame, metric){
	indices = findKNearestSimilarity(k, vec, dataFrame, metric)
	result = c()
	for(i in 1:k){
		result[i] = dataFrame1[indices[i],ncol(dataFrame1)]
	}
	return(result)
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

#return distances between a vector and a dataframe
distapply <- function(vec, dataFrame, metric){
	dist = c()
	for(row in  1:nrow(dataFrame)){
		dist[row] = metric(vec, dataFrame[row,])
	}
	return(dist)
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

#Metrics

#Euclidean Distance
euclidean <- function(vector1, vector2){
	dist = (sum((vector1 - vector2)^2))^0.5
	return(dist)
}

#Manhattan Distance
manhattan <- function(vector1, vector2){
	dist = sum(abs(vector1 - vector2))
	return(dist)
}

#Cosine Similarity
cosinesim <- function(vector1, vector2){
	sum(vector1 * vector2)/(sum(vector1^2)^0.5 * sum(vector2^2)^0.5)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
