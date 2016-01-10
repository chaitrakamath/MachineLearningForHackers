###############Non Linear Relation between columns###################
#create a sample data set where variables are not linearly related
set.seed(1)
x <- seq(-10, 10, by = 0.01)
y <- 1 - x ^2 + rnorm(length(x), 0, 5)

#if we force ggplot to select method = 'lm', the predictor line would not truly represent the underlying structure of data, which is polynomial and not linear
ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) + geom_point() + geom_smooth(method = lm, se = FALSE)

#if we let ggplot choose the method, it would select GAM for polynomial data and this would represent the underlying structure of data accurately
ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) + geom_point() + geom_smooth(se = FALSE)

#alternatively, since we know that relation between x and y is polynomial and not linear, we can transform our predictor variable to polynomial and then plot relation between transformed_x and y using linear model method
squared.x <- x^2
ggplot(data.frame(X = squared.x, Y = y), aes(x = X, y = Y)) + geom_point() + geom_smooth(method = lm, se = FALSE)

#we can also see the difference in percent of variance explained when using x vs when using transformed_x
summary(lm(y ~ x))$r.squared #around 1% of variance explained

summary(lm(y ~ squared.x))$r.squared #97% of variance explained


###############Introducing Polynomial Regression###################
set.seed(1)

#create a sine wave data set so that there is no linearity at all
x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

df <- data.frame(X = x, Y = y)

#visualize data set we created
ggplot(df, aes(x = X, y = Y)) + geom_point()

#try to fit simple linear model to this non-linear data and visualize results
lm.fit <- lm(Y ~ X, data = df)
summary(lm.fit) #58% of variance in data is explained by this model
ggplot(df, aes(x = X, y = Y)) + geom_point() + geom_smooth(method = lm, se = FALSE)

#add higher powers of X as new features to data set and create lm model again
df <- transform(df, X2 = X ^ 2)
df <- transform(df, X3 = X ^ 3)

lm.fit <- lm(Y ~ ., data = df)
summary(lm.fit) #97% of variance in data is explained by this model
df <- transform(df, predictedY = predict(lm.fit))
ggplot(df, aes(x = X, y = predictedY)) + geom_point() + geom_line()

#add higher powers of X as new features to data set and create lm model again using poly function
x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

df <- data.frame(X = x, Y = y)

poly1.fit <- lm(Y ~ poly(X, degree = 1), data = df)
summary(poly1.fit) #57% of variance in data explained by model
ggplot(df, aes(x = X, y = predict(poly1.fit))) + geom_point() + geom_line()

poly3.fit <- lm(Y ~ poly(X, degree = 3), data = df)
summary(poly3.fit) #97% of variance in data explained by model
ggplot(df, aes(x = X, y = predict(poly3.fit))) + geom_point() + geom_line()


poly5.fit <- lm(Y ~ poly(X, degree = 5), data = df)
summary(poly5.fit) #97% of variance in data explained by model
ggplot(df, aes(x = X, y = predict(poly5.fit))) + geom_point() + geom_line()

poly25.fit <- lm(Y ~ poly(X, degree = 25), data = df)
summary(poly25.fit) #98% of variance in data explained by model
ggplot(df, aes(x = X, y = predict(poly25.fit))) + geom_point() + geom_line()


#########Methods to prevent overfitting -crossvalidation#############
#simple example to explore cross-validation
set.seed(1)
x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

#create sample of indices
n <- length(x)
indices <- sort(sample(1:n, round(n * 0.5)))

#slice x and y data by indices to create train and test versions
x.train <- x[indices]
y.train <- y[indices]

x.test <- x[-indices]
y.test <- y[-indices]

#create train and test data sets from train and test versions of x and y
training.df <- data.frame(X = x.train, Y = y.train)
testing.df <- data.frame(X = x.test, Y = y.test)

#function to compute rmse
rmse <- function(y, y.hat){
	return (sqrt(mean((y - y.hat) ^ 2)))
}

#do cross-validation to get optimal polynomial degree
performance <- data.frame()

for (d in 1:12){
	poly.fit <- lm(Y ~ poly(X, d), data = training.df)
	
	performance <- rbind(performance, data.frame(Degree = d, Data = 'Train', RMSE = rmse(training.df$Y, predict(poly.fit))))
	
	performance <- rbind(performance, data.frame(Degree = d, Data = 'Test', RMSE = rmse(testing.df$Y, predict(poly.fit, newdata = testing.df))))
}

ggplot(performance, aes(x = Degree, y = RMSE, lineType = Data)) + geom_point() + geom_line(aes(colour = Data))


###############Preventing overfitting with regularization##########
#fit a simple linear model and compute its L1 and L2 norm complexity
lm.fit <- lm(y ~ x)
l2.norm.complexity <- sum(coef(lm.fit) ^ 2)
l1.norm.complexity <- sum(abs(coef(lm.fit)))

#set up data for regularization
df <- data.frame(X = x, Y = y)
x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.01)

n <- length(x)
indices <- sort(sample(1:n, round(0.5 * n)))

x.train <- x[indices]
y.train <- y[indices]

x.test <- x[indices]
y.test <- y[indices]

train.df <- data.frame(X = x.train, Y = y.train)
test.df <- data.frame(X = x.test, Y = y.test)

rmse <- function(y, y.hat){
	return (sqrt(mean((y - y.hat) ^ 2)))
}

#Implement lm with regularization using glmnet package
library(glmnet)
glmnet.fit <- with (train.df, glmnet(x = poly(X, degree = 10), y = Y))
lambdas <- glmnet.fit$lambda

#perform cross validation to get optimal value of lambda to use
performance <- data.frame()
for (lambda in lambdas){
	performance <- rbind(performance, data.frame(Lambda = lambda, RMSE = rmse(test.df$Y, with(test.df, predict(glmnet.fit, newx = poly(X, 10), s = lambda)))))
}

ggplot(performance, aes(x = Lambda, y = RMSE)) + geom_point() + geom_line()

#with best lambda in place, implement glmnet on whole data set
best.lambda <- performance$Lambda[performance$RMSE == min(performance$RMSE)]
glmnet.fit <- with (df, glmnet(poly(X, degree = 10), Y))
coef(glmnet.fit, s = best.lambda)

###############Text regression###################
#load data set and create corpus
library(tm)
ranks <- read.csv('data/oreilly.csv', stringsAsFactors = FALSE)
document.description <- data.frame(text = ranks$Long.Desc)

corpus <- Corpus(VectorSource(document.description$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('en'))
dtm <- DocumentTermMatrix(corpus)
#create x and y sets to be passed to glmnet
x <- as.matrix(dtm)
y <- ranks$Rank

set.seed(1)
performance <- data.frame()

for (lambda in c(0.1, 0.25, 0.5, 1, 2, 5)){
	for (i in 1:50){
		#create indices for training and testing data
		indices <- sample(1:100, 80)
		
		#split train and test data sets
		x.train <- x[indices, ]
		y.train <- y[indices]
		
		x.test <- x[-indices, ]
		y.test <- y[-indices]
		
		#implement glmnet model, predict on test data and compute error
		glmnet.fit <- glmnet(x.train, y.train)
		y.hat <- predict(glmnet.fit, newx = x.test, s = lambda)
		rmse <- sqrt(mean((y.test - y.hat) ^ 2))
		
		#add new row of data to performance df
		performance <- rbind(performance, data.frame(Lambda = lambda, Iteration = i, RMSE = rmse))
	}
}

ggplot(performance, aes(x = Lambda, y = RMSE)) + stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') + stat_summary(fun.data = 'mean_cl_boot', geom = 'point')
###############Logistic regression to rescue###################
#Since linear regression did not provide good results, we would model our problem as a classification problem. We would want to answer if given a set of features for a book, would it be popular or not

#create a label data for our new problem
y <- rep(c(1, 0), each = 50)

#fit a classification model and predict output on test data
regularized.fit <- glmnet(x, y, family = 'binomial')
predict(regularized.fit, newx = x, s = 0.001) #gives +ve and -ve numerical output

library(boot)
inv.logit(predict(regularized.fit, newx = x, s = 0.001))

#compute best model by using cross validation
performance <- data.frame()
set.seed(1)
for (i in (1:250)){
	#get indices for train and test sets
	indices <- sort(sample(1:100, 80))
	
	#split train and test sets
	x.train <- x[indices, ]
	y.train <- y[indices]
	
	x.test <- x[-indices, ]
	y.test <- y[-indices]
	
	#select different lambda values
	for (lambda in c(0.0001, 0.001, 0.0025, 0.05, 0.01, 0.025, 0.5, 0.1)) {
		#fit the model using glmnet classification
		glm.fit <- glmnet(x = x.train, y = y.train, family = 'binomial')
		
		#make predictions in terms of probabilites
		predictedY <- inv.logit(predict(glm.fit, newx = x.test, s = lambda))
		y.hat <- ifelse(predictedY < 0.5, 0, 1)
		
		#compute classification error
		error <- mean(y.hat != y.test)
		
		#add all values to performance data frame
		performance <- rbind(performance, data.frame(Iteration = i, Lambda = lambda, Error = error))		
	}
	
}

#plot the output
ggplot(performance, aes(x = Lambda, y = Error)) + stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar')+
stat_summary(fun.data = 'mean_cl_boot', geom = 'point') + scale_x_log10()
