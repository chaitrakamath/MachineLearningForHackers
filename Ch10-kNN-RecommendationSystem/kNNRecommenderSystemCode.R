getwd()
setwd('/Users/admin/Documents/GitHubCode/MLForHackersCode/Ch10-kNN-RecommendationSystem')
getwd()

#############k-Nearest Neighbors Algorithm##############
#load sample data
df <- read.csv('data/example_data.csv')
head(df)

#compute distance matrix
distance.matrix <- function(df){
	#create a matrix of size nrow(df) X nrow(df) filled with NAs for now
	distance <- matrix(data = rep(NA, nrow(df) ^ 2), nrow = nrow(df) )
	
	for (i in 1:nrow(df)){
		for (j in 1:nrow(df)){
#distance at [i,j] location is sqrt of sum of squared difference between their X co-ordinates (at [i, X] and [j, X]) and Y co-ordinates (at [i, Y] and [j, Y])
			distance[i, j] = sqrt((df[i, 'X'] - df[j, 'X']) ^ 2 + (df[i, 'Y'] - df[j, 'Y']) ^ 2)
		}
	}
	return (distance)
}

#function to extract k-nearest neighbors for each point in distance matrix
k.nearest.neighbors <- function(i, distance, k = 5){
	
	#sort distances
	ordered.distances <- order(distance[i, ])
	
	#return all k points except for the ith point itself
	return (ordered.distances[2:(k+1)])
}

#implement knn algorithm based on the two functions above
knn <- function(df, k = 5){
	#compute distance matrix
	distances <- distance.matrix(df)
	
	#create an dummy vector of NAs to contain all predictions
	predictions <- rep(NA, nrow(df))
	
	for (i in 1:nrow(df)){
		
		#get indices of neares neighbors
		indices <- k.nearest.neighbors(i, distances, k)
		
		#get mean of Labels for all the neighbors to a given point. If the mean is greater than 0.5, predict 1. Else, predict 0 for i
		predictions[i] <- ifelse(mean(df[indices, 'Label']) > 0.5, 1, 0)
	}
	
	return (predictions)
}

#test function
df <- transform(df, kNNPredictions = knn(df))

#compute misclassification rate
sum(df$Label != df$kNNPredictions) / nrow(df) * 100

#####black box implementation of knn algorithm using R package function####

#since we are going to use knn() function from R package, remove the knn function we defined earlier
rm('knn')
library(class)

#read data
df <- read.csv('data/example_data.csv')

#create training and test sets for feature and response variables
n <- nrow(df)
indices <- sort(sample(1:n, size = n / 2))

training.x <- df[indices, 1:2] #first two columns are feature columns
test.x <- df[-indices, 1:2]
training.y <- df[indices, 3] #third column is label
test.y <- df[indices, 3]

#find k-nearest neighbors
predicted.y <- knn(train = training.x, test = test.x, cl = training.y, k = 5)

#compute misclassification rate
sum(predicted.y != test.y) / nrow(training.x) * 100

#############R Package Installation Data##############
library(class)
library(reshape)

#load data
installations <- read.csv('data/installations.csv')
head(installations)

#transform data into wide form
user.package.matrix <- cast(data = installations, formula = User ~ Package, value = 'Installed')

#set row names of user.package.matrix to the user numbers and remove user number column from user.package.matrix
row.names(user.package.matrix) <- user.package.matrix[, 1]
user.package.matrix <- user.package.matrix[, -1]

#find correlation / similarity between packages
correlationMat <- cor(user.package.matrix)

#translate correlations into distances. 
#case1: when similarity between packages == 1 (perfectly similar), (similarity/2 + 0.5) is 1 and log(1) = 0. So, distance between perfectly similar packages is 0
#case2: when similarity between packages == - 1 (perfectly dissimilar), (similarity/2 + 0.5) is 0 and log(0) = Inf. So, distance between perfectly dissimilar packages is infinite
distances <- -log((correlationMat / 2) + 0.5)

#find k nearest neighbors of a package
k.nearest.neighbors <- function(i, distances, k = 25){
	
	#sort distances of neighbors for a given package # i
	ordered.distances <- order(distances[i, ])
	
	#return all k nearest neighbors of current package
	return(ordered.distances[2:(k + 1)])
}

#predict probability of a package being installed
installation.prob <- function(user, package, user.package.matrix, distances, k = 25){
	
	#find closest neighbor of a given package
	neighbors <- k.nearest.neighbors(package, distances, k)
	
	#find probability of that package being installed
	probs <- sapply(neighbors, function(n) {user.package.matrix[user, n]})
	return(mean(probs))
	
}

#test the function
installation.prob(1, 1, user.package.matrix, distances)

#instead of providing probability of a package being installed, recommend names of packages that are most likely to be installed
most.probable.packages <- function(user, user.package.matrix, distances, k = 25){
	#find probability of installation for all packages
	
	prob <- sapply(1:ncol(user.package.matrix), function(pkg){
		installation.prob(user, pkg, user.package.matrix, distances, k)
	})
	
	#order results from highest probable to least probable
	res <- order(prob, decreasing = TRUE)
	
	return(res)
}

#test the function
user <- 1
listing <- most.probable.packages(user, user.package.matrix, distances)
colnames(user.package.matrix)[listing[1:10]]