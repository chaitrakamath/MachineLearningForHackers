library(tm)
library(ggplot2)
library(plyr)
library(reshape)

getwd()
setwd('/Users/admin/Documents/GitHubCode/MLForHackersCode/Ch03-Classification-SpamFiltering/data')
getwd()

#function to extract content of email
msg.full <- function(path){
	con <- file(path, open = 'rt', encoding = 'latin1')
	msg <- readLines(con)
	close(con)
	return (msg)
}

#function to get sender of email
get.sender <- function(msg.vec){
	#get the line that has sender's email address
	from <- msg.vec[grep(pattern = 'From:', x = msg.vec)]
	
	#split the line by possible set of separators
	from <- strsplit(x = from, split = '[:"<>]')[[1]]
	
	#remove any empty spaces
	from <- from[which(from != " " & from != "")]
	
	#extract email address by looking for any string with '@'
	sender <- from[grep(pattern = '@', x = from)]
	
}

#function to extract message body of email
get.msg <- function(msg.vec){
	#get message by looking at the very first empty line and extracting all lines after that empty line
	msg <- msg.vec[seq(which(msg.vec == "")[1] + 1, length(msg.vec))]
	return (paste(msg, collapse = '\n'))
}

#function to extract subject of email
get.subj <- function(msg.vec){
	subj <- msg.vec[grep('Subject:', msg.vec)]
	#since subject is optional, we may have emails without any
	if (length(subj) > 0){
		#split subject into two parts: part1 will contain keyword Subject: and part2 will contain content of subject. Return the content of subject
		return(strsplit(subj, 'Subject:')[[1]][2])
		
	}
	else {
		return ("")
	}
}

#function to extract date from email
get.date <- function(msg.vec){
	#extract first line starting with 'Date:'
	date <- msg.vec[grep(pattern = '^Date:', msg.vec)[1]]
	
	#dates may be split by +, - or :
	date <- strsplit(date, '\\+|\\-|: ')[[1]][2]
	
	#strip any leading or trailing white spaces
	date <- gsub(pattern = '^\\s+|\\s+$', replacement = "", x = date)
	
	#return only first 25 characters of date as that would include date and time stamp. Anything over 25 chars should be ignored
	return(strtrim(date, width = 25))	
}

#combine all the above functions to parse through email and get vector of date, sender, subject, message and file path for each email file
parse.email <- function(path){
	full.msg <- msg.full(path)
	date <- get.date(full.msg)
	sender <- get.sender(full.msg)
	subj <- get.subj(full.msg)
	msg <- get.msg(full.msg)
	return (c(date, sender, subj, msg, path))
}

#create dataset of parsed easy ham emails 
easyham.path <- paste0(getwd(), '/easy_ham/')
easyham.docs <- dir(easyham.path) #get vector of all easyham docs
easyham.docs <- easyham.docs[easyham.docs != 'cmds'] #remove 'cmds' file

easyham.list <- lapply(easyham.docs, function(p) parse.email(path = paste0(easyham.path, p))) #create list of all parsed emails for each easyham document

easyham.mat <- do.call(rbind, easyham.list) #convert the list to matrix
easyham.df <- as.data.frame(easyham.mat, stringsAsFactors = FALSE) #convert matrix to dataframe
easyham.df$V6 <- NULL
colnames(easyham.df) <- c('Date', 'Sender', 'Subject', 'Message', 'Path') #rename columns
head(easyham.df)

#function to convert date column of easyham.df to POSIX format
date.converter <- function(dates, pattern1, pattern2){
	date.pattern1 <- strptime(dates, pattern1)
	date.pattern2 <- strptime(dates, pattern2)
	date.pattern1[is.na(date.pattern1)] <- date.pattern2[is.na(date.pattern1)]
	return (date.pattern1)
}

pattern1 <- '%a, %d %b %Y %H:%M:%S'
pattern2 <- '%d %b %Y %H:%M:%S'

easyham.df$Date <- date.converter(easyham.df$Date, pattern1, pattern2)

#convert subject and sender to lowercase
easyham.df$Subject <- tolower(easyham.df$Subject)
easyham.df$Sender <- tolower(easyham.df$Sender)

#create training dataset
priority.df <- easyham.df[with(easyham.df, order(Date)), ]
priority.train <- priority.df[1:round(nrow(priority.df) / 2), ]

##Creating weighting scheme for ranking##

#get vector of number of emails from each sender and plot it
from.weight <- melt(table(priority.df$Sender), value.name = 'Freq')
from.ex <- from.weight[from.weight$Freq > 6 & from.weight$Freq < 60, ] #cap at 6 or more emails and 60 or less emails
from.ex <- from.ex[with(from.ex, order(from.ex$Freq)), ] #order data frame
par(las = 2) #make label perpendicular to axis
par(mar = c(5, 8, 4, 8)) #increase y-axis margin
barplot(height = from.ex$Freq, horiz = T, names.arg = from.ex$Var1, cex.names = 0.3, xlab = 'Count of emails', ylab = 'Sender email')
from.ex <- transform(from.weight, Weight = log(Freq + 1))

##Weighting email thread activity
#function to find thread activity of email
find.threads <- function(email.df){
	#split email's subject by looking for string 're:'
	response.threads <- strsplit(email.df$Subject, split = 're:')
	
	#if first element of response.threads is an empty string, it is not a thread. Else, it is a thread
	is.thread <- sapply(response.threads, function(x) ifelse(x[1] == " ", TRUE, FALSE))
	
	#Extract subject if email is a thread
	thread.subj <- sapply(response.threads[is.thread], '[[', 2)
	
	#Extract sender email address if email is a thread
	thread.sender <- email.df$Sender[is.thread]
	
	#return sender and subject of thread
	return (cbind(thread.sender, thread.subj))
}

threads.matrix <- find.threads(priority.train)

#Create a weighting dataframe for each sender 
email.thread <- function(thread.matrix){
	#First column of thread.matrix gives the sender email
	senders <- thread.matrix[, 1]
	
	#Compute the number of occurrences of each sender email
	sender.freq <- table(senders)
	
	#Create a matrix of sender email, sender frequency and weight
	sender.mat <- cbind(names(sender.freq), sender.freq, log(sender.freq + 1))
	
	#Convert matrix to data frame
	sender.df <- data.frame(sender.mat, stringsAsFactors = FALSE)
	
	#Rename rows and columns of the data frame
	rownames(sender.df) <- 1:nrow(sender.df)
	colnames(sender.df) <- c('Sender', 'Freq', 'Weight')
	
	#Convert frequency and weight to numeric
	sender.df$Freq <- as.numeric(sender.df$Freq)
	sender.df$Weight <- as.numeric(sender.df$Weight)
	
	return (sender.df)
}
senders.df <- email.thread(threads.matrix)


#function to compute frequency, email weight and its log transformation of a given thread
thread.count <- function(thread, email.df){
	#find different date and time stamps of a given thread
	thread.datetime <- email.df$Date[which(email.df$Subject == thread | email.df$Subject == paste0(' re:', thread))]
	
	#find number of emails in a given thread
	freq <- length(thread.datetime)
	
	#find earliest and latest time stamps
	min.datetime <- min(thread.datetime)
	max.datetime <- max(thread.datetime)
	
	#find time span in seconds for which the thread was alive
	span <- as.numeric(difftime(max.datetime, min.datetime, units = 'secs'))
	
	#if freq is less than 2, simply return NAs
	if(freq < 2){
		return (c(NA, NA, NA))
	}
	else {
		#find average time difference between each email in a thread
		trans.weight <- freq / span
		
		#the above would be weight of email in thread. Now, compute log transformation of the weight
		log.weight <- 10 + log(trans.weight, base = 10)
		
		return (c(freq, span, log.weight))
	}
}

#function that returns freq, weight and its log for each unique thread in email.df
get.threads <- function(threads.matrix, email.df){
	threads <- unique(threads.matrix[, 2])
	thread.counts <- lapply(threads, function(t) thread.count(t, email.df))
	thread.matrix <- do.call(rbind, thread.counts)
	return (cbind(threads, thread.matrix))
}

thread.weights <- get.threads(threads.matrix, priority.train)
thread.weights <- data.frame(thread.weights, stringsAsFactors = FALSE)
names(thread.weights) <- c('Thread', 'Freq', 'Response', 'Weight')
thread.weights$Freq <- as.numeric(thread.weights$Freq)
thread.weights$Response <- as.numeric(thread.weights$Response)
thread.weights$Weight <- as.numeric(thread.weights$Weight)
thread.weights <- subset(thread.weights, !is.na(thread.weights$Freq))

#function to get frequency of each term 
term.counts <- function(term.vec, control){
	vec.corpus <- Corpus(VectorSource(term.vec))
	vec.tdm <- TermDocumentMatrix(vec.corpus, control = control)
	return (rowSums(as.matrix(vec.tdm)))
}

#find weight of each term in every subject of an email
thread.terms <- term.counts(thread.weights$Thread, control = list(stopwords = stopwords()))
thread.terms <- names(thread.terms)
term.weights <- sapply(thread.terms, function(t) mean(thread.weights$Weight[grep(pattern = t, x = thread.weights$Thread, fixed = TRUE)]))
term.weights <- data.frame(Term = thread.terms, Weight = term.weights, stringsAsFactors = FALSE)
rownames(term.weights) <- 1:nrow(term.weights)

#find weight of each term in message of each email
msg.terms <- term.counts(priority.train$Message, control = list(stopwords = stopwords(), removePunctuation = TRUE, removeNumbers = TRUE))
msg.weights <- data.frame(Term = names(msg.terms), 
								Weight = log(msg.terms, base = 										10), stringsAsFactors = FALSE, 
								row.names = 1:length(msg.terms))

###Training and testing the ranker###

#function to get weights for a search term
get.weights <- function(search.term, weight.df, term = TRUE){
	#make sure that the length of search.term is valid
	if(length(search.term) > 0){
		if(term){
			term.match <- match(names(search.term), weight.df$Term)
		}
		else {
			term.match <- match(search.term, weight.df$Thread)
		}
		match.weights <- weight.df$Weight[!is.na(term.match)]
		if (length(match.weights >= 1)){
			#return mean of weights if valid matches are found
			return(mean(match.weights))
		} 
		else {
			#return 1 if no match was found
			return (1)
		}
	}
	else {
		#return 1 if search term is not valid
		return (1)
	}
}

#function to rank message by importance
rank.message <- function(path){
	msg <- parse.email(path)
	
	#weigh based on frequency of emails from a sender
	from <- ifelse(length(which(from.weight$Sender == msg[2])) > 0, from.weight$Weight[which(from.weight$Sender == msg[2])], 1)
	
	#weight based on senders in threads and number of threads
	thread.from <- ifelse(length(which(senders.df$Sender == msg[2])) > 0, senders.df$Weight[which(senders.df$Sender == msg[2])], 1)
	
	subj <- strsplit(tolower(msg[3]), split = 're: ')
	is.thread <- ifelse(subj[1][1] == "", TRUE, FALSE)
	if(is.thread){
		activity <- get.weights(subj[1][2], thread.weights, term = FALSE)
	}
	else {
		activity <- 1
	}
	
	#compute weight based on terms in threads
	thread.terms <- term.counts(msg[3], control = list(stopwords = stopwords()))
	thread.terms.weights <- get.weights(thread.terms, term.weights)
	
	#compute weight based on terms in all messages
	msg.terms <- term.counts(msg[4], control = list(stopwords = stopwords(), removePunctuation = TRUE, removeNumbers = TRUE))
	msg.weights <- get.weights(msg.terms, msg.weights)
	
	#compute rank by interacting all the weights
	rank <- prod(from, thread.from, activity, thread.terms.weights, msg.weights)
	
	return (c(msg[1], msg[2], msg[3], rank))
}

#divide priority.df into half to create train and test sets
train.paths <- priority.df$Path[1:round(nrow(priority.df) / 2)]
train.ranks <- lapply(train.paths, rank.message)
train.ranks.matrix <- do.call(rbind, train.ranks)
train.ranks.matrix <- cbind(train.paths, train.ranks.matrix)
train.ranks.df <- data.frame(train.ranks.matrix, stringsAsFactors = FALSE)
names(train.ranks.df) <- c('Message', 'Date', 'Sender', 'Subj', 'Rank')
train.ranks.df$Rank <- as.numeric(train.ranks.df$Rank)

#set priority threshold
priority.threshold <- median(train.ranks.df$Rank)

#Set priority to 1 for message where rank is greater than the median
train.ranks.df$Priority <- ifelse(train.ranks.df$Rank >= priority.threshold, 1, 0)

#testing the ranker
test.paths <- priority.df$Path[(round(nrow(priority.df) / 2) + 1): nrow(priority.df)]
test.paths <- test.paths[-c(243:250, 1065)]
test.ranks <- lapply(test.paths, rank.message)
test.ranks.matrix <- do.call(rbind, test.ranks)
test.ranks.matrix <- cbind(test.paths, test.ranks.matrix)
test.ranks.df <- data.frame(test.ranks.matrix, stringsAsFactors = FALSE)
names(test.ranks.df) <- c('Message', 'Date', 'Sender', 'Subj', 'Rank')
test.ranks.df$Rank <- as.numeric(test.ranks.df$Rank)

#Set priority to 1 for message where rank is greater than the priority.threshold computed earlier on training dataset
test.ranks.df$Priority <- ifelse(test.ranks.df$Rank >= priority.threshold, 1, 0)

#combine all the data sets
final.df <- rbind(train.ranks.df, test.ranks.df)
final.df$Date <- date.converter(final.df$Date, pattern1, pattern2)
final.df <- final.df[rev(with(final.df, order(Date))), ]
