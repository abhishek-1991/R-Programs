######
# Bisecting K-Means and KNN Classifier
# Rename this file before submitting 
#####

require(RColorBrewer)
require(class)
require(caret)

# set seed to ensure consistent results
set.seed(100)


################################################
#   TA defined functions
#   Code has already been provided for you - you don't need to modify this 
###############################################
# Plot clustering result
plot_clustering_result <- function(data.df, result, title, k){
  # Plot using two columns in the data
  plot(data.df[, c(2,4)], col = brewer.pal(k, "Set1")[result[[1]]], pch = '.',
       cex = 3, main = title)
}

################################################
# GRADED FUNCTIONS
# Write your code using the given function definitions
# Input and output types are defined for each function
###############################################

# Implement bisecting k means.
# Input:
# data.df: data frame loaded using load_data() function
# iter.max: Max. number of trials for kmeans, as per Algorithm 8.2 in textbook. 
# You are allowed to use the pre-defined kmeans() function.
# k: Number of clusters to find in bisecting k-means

# Output:
# Your output/function return value will be a list containing 2 elements
# first element of the list is a vector containing cluster assignments (i.e., values 1 to k assigned to each data point)
# second element of the list is a vector containing SSE of each cluster
# Additional Information:
# When identifying which cluster to split, choose the one with maximum SSE
# When performing kmeans, pick two random points the cluster with largest SSE as centers at every iteration. 
# Ensure that the two centers being randomly picked are not the same.
# terminating condition: when k clusters have been found
# bisecting k means 
bkm <- function(data.df, iter.max, k){
  # Hint: Remember, the first column is of type ID. 
  # Don't use this column while clustering
  data.frame = data.df[,2:ncol(data.df)]
  Integer.max = .Machine$integer.max
  fit.model <- kmeans(x=data.frame,1,algorithm="Lloyd")
  cluster.table <- data.frame("cluster"=1,"sse"=fit.model$withinss)
  cluster.spread = fit.model$cluster
  while(nrow(cluster.table)<k)
  {
    max.sse.cluster <- cluster.table[1,]
    cluster.table <- cluster.table[-1,]
    cluster.table$cluster[cluster.table$cluster > max.sse.cluster$cluster] <- cluster.table$cluster[cluster.table$cluster > max.sse.cluster$cluster] - 1
    temp.data <- data.frame[cluster.spread == max.sse.cluster$cluster,]
    cluster.spread[cluster.spread == max.sse.cluster$cluster] <- Integer.max
    cluster.spread[cluster.spread > max.sse.cluster$cluster & cluster.spread != Integer.max] <- cluster.spread[cluster.spread > max.sse.cluster$cluster & cluster.spread != Integer.max] - 1
    min.tot.sse <- Integer.max
    for(i in 1:iter.max)
    {
      fit <- kmeans(temp.data,2,algorithm = "Lloyd")
      if(fit$tot.withinss < min.tot.sse)
      {
        fit.min <- fit
        min.tot.sse = fit$tot.withinss
      }
    }
    
    if(nrow(cluster.table)==0)
    {
      cluster.spread <- fit.min$cluster
      cluster.table <- data.frame("cluster"=1,"sse"=fit.min$withinss[1])
      cluster.table <- rbind(cluster.table,c(2,fit.min$withinss[2]))
    }
    else
    {
      max.cluster.num <- max(cluster.table$cluster)
      cluster.table <- rbind(cluster.table,c(max.cluster.num + 1,fit.min$withinss[1]))
      cluster.table <- rbind(cluster.table,c(max.cluster.num + 2,fit.min$withinss[2]))
      for(i in 1:nrow(data.frame))
      {
        for(j in 1:nrow(temp.data))
        {
          if((data.frame[i,1]==temp.data[j,1] & data.frame[i,2]==temp.data[j,2] & data.frame[i,3]==temp.data[j,3] & data.frame[i,4]==temp.data[j,4]) & cluster.spread[i]==Integer.max)
          {
            if(fit.min$cluster[j] == 1)
            {cluster.spread[i]=max.cluster.num + 1}
            if(fit.min$cluster[j] == 2)
            {cluster.spread[i]=max.cluster.num + 2}
          }
        }
      }
    }
    cluster.table <- cluster.table[order(-cluster.table$sse),]
  }
  return(list(cluster.spread,cluster.table$sse))
}


# Write code for comparing kmeans with result from bisecting kmeans here - Part b
# Input:
# data.df:  Dataset used for kmeans/bisecting kmeans clustering 
# Result: Variable of type list, obtained on running bkm() function
# k : k value for k-means
# km_centers: ID values of centers for KMeans

#Returns:
# Nothing, just print the observations requested

kmeans_comparison <- function(data.df, result, k, km_centers){
    # First, run KMeans using km_centers and k. 
    # Compare outcomes of kmeans with bisecting kmeans in terms of:
    # 1. Overall SSE
    # 2. Plot the clusters and compare the size and shape of clusters generated
    # 3. Using the plot, also verify (visually) if points are being assigned to different clusters
  fit <- kmeans(data.df[,2:ncol(data.df)],centers = data.df[km_centers,2:ncol(data.df)])
  print(paste("Overall SSE of K-means: ",fit$tot.withinss))
  print(paste("Overall SSE of Bisecting K-means: ",sum(result[[2]])))
  plot_clustering_result(data.df,list(fit$cluster,fit$withinss),"KMeans Outcome",k)
}

# Write code for KNN algorithm
# implement my_knn with euclidean distance, majority vote and 
# randomly resolving ties
# you are NOT allowed to use the built-in knn function or the dist() function
# (i.e., implement the K Nearest Neighbors, also implement a function for euclidean distance calculation)

# Input: 
# train: training data frame
# test: test data frame
# cl: class labels for training data
# k: 'k' value in KNN

# Output:
# A vector of class labels. return variable should be of type factor

#euclidian distance implementation
euclid.dist <- function(x1,x2)
{
  return(sqrt(sum((x1-x2)^2)))
}

#Function to return the most frequent element in a list
calc.mode <- function(x)
{
  temp <- unique(x)
  return(temp [which.max(tabulate(match(x, temp )))])
}

my_knn <- function(train, test, cl, k)
{
  test.cl <- list()
  for(i in 1:nrow(test))
  {
    element <- test[i,]
    dist.table <- data.frame(distance=numeric(),class=character())
    for(j in 1:nrow(train))
    {
     distance <- euclid.dist(train[j,],element)
     dist.table <- rbind(dist.table,data.frame("distance"=distance,"class"=cl[j]))
    }
    dist.table <- dist.table[order(dist.table$distance),]
    element.class <- calc.mode(dist.table$class[1:k])
    test.cl <- append(test.cl,as.character(element.class))
    rm(dist.table)
  }
  return(factor(unlist(test.cl,recursive = FALSE)))
}

# Generate accuracy measures for your KNN classification
# Input:
# test_cl: actual class labels for test data
# knn_result: predicted class labels for test data

# Output:
# A vector of size 4 in the following order: 
# (overall accuracy, precision for the class 'setosa', recall for the class 'setosa', F-measure for the class 'setosa')

# DONOT use predefined libraries to calculate the accuracy measures
# Implement the formulae by generating the confusion matrix using the table() function
my_knn_accuracy <- function(test_cl, knn_result){
 
  accuracy.total <- length(knn_result[knn_result==test_cl])/length(knn_result)
  
  confusion.matrix <- table(test_cl,knn_result)
  tp <- confusion.matrix[1,1]
  fp <- confusion.matrix[1,2]+confusion.matrix[1,3]
  fn <- confusion.matrix[2,1]+confusion.matrix[3,1]
  
  precision.setosa <- tp/(tp+fp)
  recall.setosa <- tp/(tp+fn)
  
  f1.setosa <- (2*precision.setosa*recall.setosa)/(precision.setosa+recall.setosa)
  
  return(c(accuracy.total,precision.setosa,recall.setosa,f1.setosa))
}
