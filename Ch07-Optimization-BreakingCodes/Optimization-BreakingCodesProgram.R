getwd()
setwd('/Users/admin/Documents/GitHubCode/MLForHackersCode/Ch07-Optimization-BreakingCodes/')
getwd()
###################Introduction to Optimization#####################
#load data set
height.weight <- read.csv('data/01_heights_weights_genders.csv')

#function to get predicted weight given height, intercept a and slope b
height.to.weight <- function(height, a, b){
	weight <- a + b * height
	return(weight)
}

#function to compute squared error 
squared.error <- function(height.weight, a, b){
	predicted_weight <- height.to.weight(height.weight$Height, a, b)
	error <- height.weight$Weight - predicted_weight
	return (sum(error ^ 2))
}

#Try to find optimal values of a and b within a given range
for (a in seq(-1, 1, by = 1)){
	for (b in seq(-1, 1, by = 1)){
		print (squared.error(height.weight, a, b))
	}
}

#finding optimum values of a and b using optim function
optim(c(0, 0), function(x){squared.error(height.weight, x[1], x[2])})

#trying to find optimal value of a by fixing b = 0
a.error <- function (a){
	return (squared.error(height.weight, a, 0))
}

curve(sapply(x, function (a) {a.error(a)}), from = -1000, to = 1000)

#trying to find optimal values of b by fixing a = 0
b.error <- function(b){
	return (squared.error(height.weight, 0, b))
}

curve(sapply(x, function(b) {b.error(b)}), from = -1000, to = 1000)
###################Ridge Regression#####################
#function to compute error with ridge regression applied
ridge.error <- function(height.weight, a, b, lambda){
	predicted_weight <- height.to.weight(height.weight$Height, a, b)
	error <- predicted_weight - height.weight$Weight
	return (sum(error ^ 2) + lambda * (a ^ 2 + b ^ 2))
}

#finding optimum value of a and b using optim function at lambda = 1
lambda <- 1 #assume this is the best value of lambda from CV
optim(c(0, 0), function(x){ridge.error(height.weight, x[1], x[2], lambda)})

#trying to find optimal values of a by fixing b = 0
a.ridgeError <- function(a, lambda){
	return (ridge.error(height.weight, a, 0, lambda))
}

curve(sapply(x, function(a) {a.ridgeError(a, lambda)}), from = -1000, to = 1000)

#trying to find optimal values of b by fixing a = 0
b.ridgeError <- function(b, lambda){
	return (ridge.error(height.weight, 0, b, lambda))
}

curve(sapply(x, function(b){b.ridgeError(b, lambda)}), from = -1000, to = 1000)

#function to compute absolute error 
absolute.error <- function(height.weight, a, b){
	predicted_weight <- height.to.weight(height.weight$Height, a, b)
	error <- predicted_weight - height.weight$Weight
	return (sum(abs(error)))
}

#trying to find optimal values of a by fixing b = 0
a.absoluteError <- function(a){
	return (absolute.error(height.weight, a, 0))
}

curve(sapply(x, function(a){a.absoluteError(a)}), from = -1000, to = 1000)

#trying to find optimal values of b by fixing a = 0
b.absoluteError <- function(b){
	return (absolute.error(height.weight, 0, b))
}

curve(sapply(x, function(b){b.absoluteError(b)}), from = -1000, to = 1000)
###################Code Breaking as Optimization#####################

#build Caesar's cipher
english.letters <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
caesar.cipher <- list()
inv.caesar.cipher <- list()

for (i in 1:length(english.letters)){
	caesar.cipher[[english.letters[i]]] <- english.letters[i %% 26 + 1]
	inv.caesar.cipher[[english.letters[i %% 26 + 1]]] <- english.letters[i]
}
print (caesar.cipher)

apply.cipher.to.string <- function(string, cipher){
	output <- ''
	for (i in 1:nchar(string)){
		output <- paste(output, cipher[[substr(string, i, i)]], sep = '')
	}
	return (output)
}

apply.cipher.to.text <- function(text, cipher) {
	output <- c()
	
	for (string in text){
		output <- c(output, apply.cipher.to.string(string, cipher))
	}
	return (output)	
}

apply.cipher.to.text (c('sample', 'text'), caesar.cipher)

#function to generate random cipher
generate.random.cipher <- function(){
	cipher <- list()
	
	inputs <- english.letters
	outputs <- english.letters[sample(1:length(english.letters), length(english.letters))]
	
	for (i in 1:length(english.letters)){
		cipher[[inputs[i]]] <- outputs[i]
	}
	
	return (cipher)
}

modify.cipher <- function(cipher, input, output){
	new.cipher <- cipher
	new.cipher[[input]] <- output
	old.output <- cipher[[input]]
	collateral.input <- names(which(sapply(names(cipher), function(key){ cipher[[key]]}) == output))
	new.cipher[[collateral.input]] <- old.output
	return (new.cipher)
}

propose.modified.cipher <- function(cipher){
	input <- sample(names(cipher), 1)
	output <- sample(english.letters, 1)
	return (modify.cipher(cipher, input, output))
}

#load lexical database data
load('data/lexical_database.Rdata')
lexical.database[['a']]
lexical.database[['the']]
lexical.database[['he']]
lexical.database[['she']]
lexical.database[['his']]
lexical.database[['her']]

#function to compute probability of each word given a lexical database
one.gram.prob <- function(one.gram, lexical.database = list()){
	lexical.prob <- lexical.database[[one.gram]]
	
	if (is.null(lexical.prob) || is.na(lexical.prob) ){
		return (.Machine$double.eps)
	}
	else{
		return (lexical.prob)
	}
}

#function to compute log probability of text 
log.prob.text <- function(text, cipher, lexical.database = list()){
	log.prob <- 0.0
	
	for (one.gram in text){
		decrypted.word <- apply.cipher.to.string(one.gram, cipher)
		log.prob <- log.prob + one.gram.prob(one.gram, lexical.database)
	}
	
	return (log.prob)
}

#function for Metropolis method
metropolis.step <- function(text, cipher, lexical.database = list()){
	proposed.cipher <- propose.modified.cipher(cipher)
	
	log.prob1 <- log.prob.text(text, cipher, lexical.database)
	log.prob2 <- log.prob.text(text, proposed.cipher, lexical.database)
	
	if (log.prob2 > log.prob1){
		return (proposed.cipher)
	}
	
	else {
		a <- exp(log.prob2 - log.prob1)
		x <- runif(1)
		if (x < a){
			return (proposed.cipher)
		}
		else{
			return (cipher)
		}
	}
}

#test out the functions
decrypted.text <- c('here', 'is', 'a', 'sample', 'text')
encrypted.text <- apply.cipher.to.text(decrypted.text, caesar.cipher)

set.seed(1)
cipher <- generate.random.cipher()
results <- data.frame()
iter <- 500
for (i in 1:iter){
	log.prob <- log.prob.text(encrypted.text, cipher, lexical.database)
	current.decrypted.text <- paste(apply.cipher.to.text(encrypted.text, cipher), collapse = ' ')
	correct.text <- as.numeric(current.decrypted.text == paste(decrypted.text, collapse = ' '))
	results <- rbind(results, data.frame(Iteration = i, logProb = log.prob, currentDecryptedText = current.decrypted.text, correctText = correct.text))
	cipher <- metropolis.step(encrypted.text, cipher, lexical.database)
}

write.table(results, file = 'data/results.csv', row.names = FALSE, sep = '\t')