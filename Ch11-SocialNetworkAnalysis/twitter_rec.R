#since we are going to be using functions from original snowballSamplingCode.R file, load the file
source('snowballSamplingCode.R')

#load required packages 
library(igraph)

#read data
user <- 'johnmyleswhite'

user.graph <- read.graph(paste('data/', user, '_net.graphml', sep = ''), format = 'graphml')

#find name of all friends of the seed user
friends <- V(user.graph)$name[neighbors(user.graph, user, mode = 'out')]

#generate complete edge list for the graph
user.el <- get.edgelist(user.graph)

#find all users who are not the original seed user and who the seed user is not following, but the seed user's friends are following
non.friends <- sapply(1:nrow(user.el), function(i) {
#	current user name should not be the seed, seed should not be already following the user and user should be followed by friends of the seed
	ifelse(any(user.el[i, ] == user | !user.el[i, 1] %in% friends) | user.el[i, 2] %in% friends, FALSE, TRUE)
})


non.friends.el <- user.el[which(non.friends == TRUE), ]
friends.count <- table(non.friends.el[, 2])

#in order to report the results, we would first createa a dataframe that provides information about the users that friends of the seed user are following (but seed is not following)
friends.followers <- data.frame(list(Twitter.Users = names(friends.count), Friends.Following = as.numeric(friends.count)), stringsAsFactors = FALSE)

#for each user, compute percentage of friends of seed that are following that user
friends.followers$Friends.Norm <- friends.followers$Friends.Following / length(friends)

#Sort the data frame in descending order of percentage of friends following a user
friends.followers <- friends.followers[with(friends.followers, order(-Friends.Norm)), ]

friends.followers[1:10, ]

#instead of only recommending friends of friends to seed user, we could also try to recommend users who match particular interests of seed user or fit on the same dimension as the seed user

user.ego <- read.graph(paste0('data/', user, '_ego.graphml'), format = 'graphml')
friends.partitions <- cbind(V(user.ego)$HC8, V(user.ego)$name)

#function to recommend a potential friend to follow based on partition and number of friends of the seed that follow the potential friend within that partition
partition.follows <- function(i){
	
	#extract all friends for a given partition number
	friends.in <- friends.partitions[which(friends.partitions[, 1] == i), 2]
	
	#find those friends within the partition that the seed user is not following currently
	partition.non.follow <- non.friends.el[which(!is.na(match(non.friends.el[, 1], friends.in))), ]
		
	#if no match is found, return NA
	if(nrow(partition.non.follow) < 2){
		return(c(i, NA))
	}
	#else, return user followed the most in the current partition
	else{
		#get number of followers of each user in parition.non.follow
		partition.fav <- table(partition.non.follow[, 2])
		
		#re-order from most popular to least popular
		partition.fav <- partition.fav[order(-partition.fav)]
		
		#return name of most popular user
		return(c(i, names(partition.fav)[1]))
	}	
}

#apply above function to each partition in our data
partition.recs <- t(sapply(unique(friends.partitions[, 1]), partition.follows))

#remove any NAs or duplicated recommendations
partition.recs <- partition.recs[!is.na(partition.recs[, 2]) & !duplicated(partition.recs[, 2]), ]