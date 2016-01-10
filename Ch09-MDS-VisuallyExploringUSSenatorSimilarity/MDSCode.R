getwd()
setwd('/Users/admin/Documents/GitHubCode/MLForHackersCode/Ch09-MDS-VisuallyExploringUSSenatorSimilarity')
getwd()

######################Clustering based on similarity###########
#create a toy data set of customer ratings
set.seed(851982)
toy.data <- matrix(data = sample(c(-1, 0, 1), size = 24, replace = TRUE), nrow = 4, ncol = 6)
colnames(toy.data) <- c('P1', 'P2', 'P3', 'P4', 'P5', 'P6')
row.names(toy.data) <- c('U1', 'U2', 'U3', 'U4')

#create customer-by-customer matrix to understand how the customer's preferences are related
cust.cor <- toy.data %*% t(toy.data)

#find Euclidean distance between user U1 and U4 by hand
U1_U4.dist <-  sqrt(sum((cust.cor[1,] - cust.cor[4,]) ^ 2))

#create a distance matrix using R's dist function
cust.distances <- dist(cust.cor, method = 'euclidean')

#use mds on distance matrix to visualize closeness of user ratings
toy.mds <- cmdscale(d = cust.distances, k = 2)
plot(toy.mds)
text(toy.mds, c('U1', 'U2', 'U3', 'U4'))

##################How do US Senators cluster?###########

#load required packages
library(foreign)
library(ggplot2)

#set data directory and file names
data.dir <- 'data/'
data.files <- list.files(data.dir)

#load data 
rollcall.data <- lapply(data.files, function(filename) {read.dta(paste0(data.dir, filename), convert.factors = FALSE)})
str(rollcall.data[[1]])

#function to group all possible values of Yeas as 1, Nays as -1 and non-voting as 0
rollcall.simplified <- function(df){
	no.pres <- subset(df, state < 99)
	for (i in 1:ncol(no.pres)){
		#group all Yeas
		no.pres[, i] <- ifelse(no.pres[,i] > 1 & no.pres[, i] < 4, 1, no.pres[, i])
		
		#group all Nays
		no.pres[, i] <- ifelse(no.pres[,i] > 3 & no.pres[, i] < 7, -1, no.pres[, i])
		
		#group all non-voting counts
		no.pres[, i] <- ifelse(no.pres[,i] > 6, 0, no.pres[, i])
	}
	
	return(as.matrix(no.pres[, 10:ncol(no.pres)]))
}

rollcall.simple <- lapply(rollcall.data, rollcall.simplified)

#compute Euclidean distance matrix by using matrix multiplication for each dataframe in rollcall.simple
rollcall.dist <- lapply(rollcall.simple, function(mat) {dist(x = (mat %*% t(mat)), method = 'euclidean')})

#use mds on distance matrix to visualize closeness of Senator's voting
rollcall.mds <- lapply(rollcall.dist, function(d) as.data.frame(cmdscale(d, k = 2) * -1))

#add Senator identification information to rollcall.mds dataset
congress.num <- 101:111
for (i in 1:length(rollcall.mds)){
	#name columns of rollcall.mds
	names(rollcall.mds[[i]]) <- c('x', 'y')
	
	#subset rollcall.data to exclude VP votes
	congress <- subset(rollcall.data[[i]], state < 99)
	
	#get first names of all senators
	sen.names <- sapply(as.character(congress$name), function(name) strsplit(name, '[,]')[[1]][1])
	
	#add senator names, party affiliations and congress numbers to rollcall.mds
	rollcall.mds[[i]] <- transform(rollcall.mds[[i]], name = sen.names, party = as.factor(congress$party), congress = congress.num[i])
}

#plot data for 101st congress
cong.101 <- rollcall.mds[[1]]
base.101 <- ggplot(cong.101, aes(x = x, y = y)) + 
theme_bw() + 
theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), panel.grid.major = element_blank()) + ggtitle('Roll Call Vote MDS Clustering for 101st US Senate') + 
xlab('') + ylab('') + 
scale_shape(name = 'Party', breaks = c('100', '200', '328'), labels = c('Dem.', 'Rep.', 'Ind.'), solid = FALSE) +
scale_color_manual(name = 'Party', values = c('100' = 'blue', '200' = 'red', '328' = 'black'), breaks = c('100', '200', '328'), labels = c('Dem.', 'Rep.', 'Ind.'))

print(base.101 + geom_point(aes(shape = party, alpha = 0.75, size = 2)))
print(base.101 + geom_text(aes(color = party, alpha = 0.75, label = cong.101$name, size = 2)))


#create plot of all congresses
all.mds <- do.call(rbind, rollcall.mds)
all.plot <- ggplot(all.mds, aes(x = x, y = y)) + 
geom_point(aes(shape = party, alpha = 0.75, size = 2)) + 
theme_bw() + 
theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), panel.grid.major = element_blank()) + ggtitle('Roll Call Vote MDS Clustering for US Senate (101st to 111th Congress)') + 
xlab('') + ylab('') + 
scale_shape(name = 'Party', breaks = c('100', '200', '328'), labels = c('Dem.', 'Rep.', 'Ind.'), solid = FALSE) +
scale_color_manual(name = 'Party', values = c('100' = 'blue', '200' = 'red', '328' = 'black'), breaks = c('100', '200', '328'), labels = c('Dem.', 'Rep.', 'Ind.')) +
facet_wrap( ~ congress)

all.plot