getwd()
setwd("/Users/admin/Documents/GitHubCode/MLForHackersCode/Ch03-Classification-SpamFiltering")
getwd()

#############Bayesian Spam Classifier#############
#Load required packages and set up datafile paths
library(tm) #required for text mining
library(ggplot2) #required for visualizations

spam.path <- paste0(getwd(), '/data/spam/')
spam2.path <- paste0(getwd(), '/data/spam_2/')
easyham.path <- paste0(getwd(), '/data/easy_ham/')
easyham2.path <- paste0(getwd(), '/data/easy_ham_2/')
hardham.path <- paste0(getwd(), '/data/hard_ham/')
hardham2.path <- paste0(getwd(), '/data/hard_ham_2/')

#Function returns character vector of email's content / body
get.msg <- function(path){
	con <- file(path, open = 'rt', encoding = 'latin1')
	text <- readLines(con)
	#Extract message after the first full line break
	msg <- text[seq(which(text == "")[1] + 1, length(text), 1)]
	close(con)
	return (paste(msg, collapse = '\n'))
}

spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs != 'cmds')]
all.spam <- sapply(spam.docs, function(p) get.msg(paste0(spam.path, p)))

#Function returns term document matrix of a given corpus
get.tdm <- function(doc.vec){
	doc.corpus <- Corpus(VectorSource(doc.vec))
	control <- list(removePunctuation = TRUE, stopwords = TRUE, minDocFreq = 2, removeNumbers = TRUE)
	doc.tdm <- TermDocumentMatrix(doc.corpus, control)
	return (doc.tdm)
}

spam.tdm <- get.tdm(all.spam)

#Creating training data set of spam emails
spam.matrix <- as.matrix(spam.tdm) #convert tdm to matrix
spam.counts <- rowSums(spam.matrix, na.rm = TRUE) #get total count o each term across all spam emails
spam.df <- data.frame(term = names(spam.counts), frequency = as.numeric(spam.counts), stringsAsFactors = FALSE)#create a dataframe with two columns: term names and their frequency
spam.occurrence <- sapply(1:nrow(spam.matrix), function(i){length(which(spam.matrix[i, ] > 0)) / ncol(spam.matrix)}) #compute the proportion of documents in which a given term occurs. Measure against total number of documents
spam.density <- spam.df$frequency / sum(spam.df$frequency) #compute frequency of each term in entire corpus. Measure against all terms combined
spam.df <- transform(spam.df, density = spam.density, occurrence = spam.occurrence)
head(spam.df[with(spam.df, order(-occurrence)), ])

#Creating training data set of easy ham emails
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[easyham.docs != 'cmds']
easyham.docs <- sample(easyham.docs, size = 500, replace = FALSE) #choose only 500 random easy ham documents as we have 500 spam emails
all.easyham <- sapply(easyham.docs, function(p) get.msg(paste0(easyham.path, p)))
easyham.tdm <- get.tdm(all.easyham)
easyham.matrix <- as.matrix(easyham.tdm)
easyham.occurrence <- sapply(1:nrow(easyham.matrix), function(i) {length(which(easyham.matrix[i, ] > 0)) / ncol(easyham.matrix)})
easyham.count <- rowSums(easyham.matrix)
easyham.df <- data.frame(term = names(easyham.count), frequency = as.numeric(easyham.count), stringsAsFactors = FALSE)
easyham.density <- easyham.df$frequency / sum(easyham.df$frequency)
easyham.df <- transform(easyham.df, density = easyham.density, occurrence = easyham.occurrence)
head(easyham.df[with(easyham.df, order(-occurrence)), ])

###########Defining classifier and testing it with hard ham########
classify.email <- function(path, train.df, prior = 0.5, c = 1e-6){
	msg <- get.msg(path)
	msg.tdm <- get.tdm(msg)
	msg.frequency <- rowSums(as.matrix(msg.tdm))
	#find common terms in training set and new dataset
	msg.match <- intersect(x = names(msg.frequency), y = train.df$term) #returns all words from message that exist in training set
	if (length(msg.match == 0)) {
		#all words from the message are new and never seen during training. So, assign small probability
		return (prior * c^(length(msg.frequency)))
	}
	else{
		#find occurrence rate of matching terms from training dataset
		match.probs <- train.df$occurrence[match(msg.match, train.df$term)]
		#update prior as follows:
		#a) compute combined probability of matching terms by taking product of match.probs 
		#b) also, compute small probability that would be attached to the terms that are not yet seen in the training dataset
		return (prior * prod(match.probs, na.rm = TRUE) * c^(length(msg.frequency) - length(msg.match)))
	}	
}

#--Testing classifier with hard ham documents
hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[hardham.docs != 'cmds']
hardham.spamtest <- sapply(hardham.docs, function(p) classify.email(paste0(hardham.path, p), train.df = spam.df))
hardham.hamtest <- sapply(hardham.docs, function(p) classify.email(paste0(hardham.path, p), train.df = easyham.df))
hardham.res <- ifelse(hardham.spamtest > hardham.hamtest, TRUE, FALSE)
summary(hardham.res)
#Testing classifier agains all email types
spam.classifier <- function(path){
	#probability of email being spam
	pr.spam <- classify.email(path, spam.df)
	#probability of email being ham
	pr.easyham <- classify.email(path, easyham.df)
	res <- ifelse(pr.spam > pr.easyham, TRUE, FALSE)
	return (c(pr.spam, pr.easyham, res))	
}

#test the model on all types of emails
spam2.docs <- dir(spam2.path)
spam2.docs <- spam2.docs[spam2.docs != 'cmds']
spam2.test <- lapply(spam2.docs, function(p) spam.classifier(path = paste0(spam2.path, p)))

easyham2.docs <- dir(easyham2.path)
easyham2.docs <- easyham2.docs[easyham2.docs != 'cmds']
easyham2.test <- lapply(easyham2.docs, function(p) spam.classifier(path = paste0(easyham2.path, p)))

hardham2.docs <- dir(hardham2.path)
hardham2.docs <- hardham2.docs[hardham2.docs != 'cmds']
hardham2.test <- lapply(hardham2.docs, function(p) spam.classifier(path = paste0(hardham2.path, p)))

#combine all results into a single data frame
spam2test.mat <- do.call(rbind, spam2.test)
spam2test.final <- cbind(spam2test.mat, 'Spam')

easyham2test.mat <- do.call(rbind, easyham2.test)
easyham2test.final <- cbind(easyham2test.mat, 'Easy Ham')

hardham2test.mat <- do.call(rbind, hardham2.test)
hardham2test.final <- cbind(hardham2test.mat, 'Hard Ham')

class.mat <- rbind(spam2test.final, easyham2test.final, hardham2test.final)
class.df <- as.data.frame(class.mat, stringsAsFactors = FALSE)
str(class.df)
names(class.df) <- c('Pr.Spam', 'Pr.Ham', 'Class', 'Type')


