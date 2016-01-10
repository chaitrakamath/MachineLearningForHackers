getwd()
setwd('/Users/admin/Documents/GitHubCode/MLForHackersCode/Ch11-SocialNetworkAnalysis')
getwd()

library(igraph)
source('snowballSamplingCode.R')

user <- 'johnmyleswhite'

user.net <- read.graph(paste('data/', user, '_net.graphml', sep = ''), format = 'graphml')

#set up Label information for user.net as it would be later used in Gephi graph
user.net <- set.vertex.attribute(user.net, 'Label', value = get.vertex.attribute(user.net, 'name'))

#compute coreness of each node
user.cores <- graph.coreness(user.net, mode = 'in')

#get subgraph consisting of users who with core of of 2 or more
user.clean <- subgraph(user.net, which(user.cores > 1))

#extract seed user's ego network
user.ego <- subgraph(graph = user.net, v = c(neighbors(graph = user.net, v = user, mode = 'out')))

#alternately, we can extract information from the file provided
user.ego <- read.graph(file = 'data/johnmyleswhite_ego.graphml', format = 'graphml')

#compute shortest distances between all users in graph
user.sp <- shortest.paths(user.ego)

#create hierarchical clusters based on distances between users
user.hc <- hclust(dist(user.sp))
plot(user.hc)

#add clustering partition information to nodes
for (i in 2:10){
	#create a character vector of cluster assignments for each node in the data
	user.cluster <- as.character(cutree(user.hc, k = i))
	
	#since the fist node is always the seed user, set its cluster assignment to 0
	user.cluster[0] <- '0'
	
	#add cluster assignment information to the nodes
	user.ego <- set.vertex.attribute(graph = user.ego, name = paste('HC', i, sep = ''), value = user.cluster)
}


