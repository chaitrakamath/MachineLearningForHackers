#set data directories
getwd()
setwd('/Users/admin/Documents/GitHubCode/MLForHackersCode/Ch12-ModelComparison')
getwd()

#load required packages
library(e1071)
library(reshape)
library(ggplot2)
library(cvTools)
#######################Support Vector Machine########################
#read data
df <- read.csv('data/df.csv')
str(df)

#create a simple linear classification model
logit.fit <- glm(Label ~ X + Y, data = df, family = binomial(link = 'logit'))
summary(logit.fit)

#predict labels
logit.predictions <- ifelse(predict(logit.fit) > 0, 1, 0)

#compute accuracy of prediction
mean(df$Label == logit.predictions)

#create a svm model
svm.fit <- svm(Label ~ X + Y, data = df)

#predict labels
svm.predictions <- ifelse(predict(svm.fit) > 0, 1, 0)

#compute accuracy of prediction
mean(df$Label == svm.predictions)

#plot predictions by logistic classification model vs predictions by svm model
df <- cbind(df, data.frame(LogitPredictions = logit.predictions, SVMPredictions = svm.predictions))
predictions <- melt(df, id.vars = c('X', 'Y'))
ggplot(predictions, aes(x = X, y = Y, color = factor(value))) +
geom_point() + facet_grid(variable ~ .)

#create svm models with various kinds of kernels and plot their predictions against each other and the original Label column of data

#remove the added columns from df
df <- df[, c('X', 'Y', 'Label')]

#create a linear SVM, compute predictions and accuracy of predictions
linear.svm <- svm(Label ~ X + Y, data = df, kernel = 'linear')
linear.svm.predictions <- ifelse(predict(linear.svm) > 0, 1, 0)
mean(linear.svm.predictions == df$Label)

#create a polynomial SVM, compute predictions and accuracy of predictions
polynomial.svm <- svm(Label ~ X + Y, data = df, kernel = 'polynomial')
polynomial.svm.predictions <- ifelse(predict(polynomial.svm) > 0, 1, 0)
mean(polynomial.svm.predictions == df$Label)


#create a radial SVM, compute predictions and accuracy of predictions
radial.svm <- svm(Label ~ X + Y, data = df, kernel = 'radial')
radial.svm.predictions <- ifelse(predict(radial.svm) > 0, 1, 0)
mean(radial.svm.predictions == df$Label)


#create a sigmoid SVM, compute predictions and accuracy of predictions
sigmoid.svm <- svm(Label ~ X + Y, data = df, kernel = 'sigmoid')
sigmoid.svm.predictions <- ifelse(predict(sigmoid.svm) > 0, 1, 0)
mean(sigmoid.svm.predictions == df$Label)

#add all the predictions to df
df <- cbind(df, LinearSVM = linear.svm.predictions, PolynomialSVM = polynomial.svm.predictions, RadialSVM = radial.svm.predictions, SigmoidSVM = sigmoid.svm.predictions)

#create predictions data frame with all predictions and label included
predictions <- melt(df, id.vars = c('X', 'Y'))

#plot the data
ggplot(predictions, aes(x = X, y = Y, color = as.factor(value))) + 
geom_point() + facet_grid(variable ~ .)

#use cross validation to tune degree parameter for polynomial svm
df <- df[, c('X', 'Y', 'Label')]

set.seed(1)
k <- 5
prediction.accuracy <- data.frame()
folds <- cvFolds(nrow(df), K = k)


for (deg in c(1, 3, 5, 10, 12)){
	for (i in 1:k){
			train <- df[folds$subsets[folds$which != i], ]
			validation <- df[folds$subsets[folds$which == i], ]
	
			polynomial.svm.fit <- svm(Label ~ X + Y, data = train, kernel = 'polynomial', degree = deg)
			
			prediction <- ifelse(predict(polynomial.svm.fit, newdata = validation) > 0, 1, 0)
			
			accuracy <- mean(prediction == validation$Label)
			
			prediction.accuracy <- rbind(polynomial.prediction, data.frame(Degree = deg, Accuracy = accuracy))	
	}
}

aggregate( . ~ Degree, prediction.accuracy, mean) #based on the results, degree 10 seems to provide better accuracy

#fit various degrees of polynomial fit svm model to data, get accuracy rates and plot the results
poly.deg3.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'polynomial', degree = 3)
poly.deg3.predictions <- ifelse(predict(poly.deg3.svm.fit) > 0, 1, 0)
mean(poly.deg3.predictions == df$Label)

poly.deg5.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'polynomial', degree = 5)
poly.deg5.predictions <- ifelse(predict(poly.deg5.svm.fit) > 0, 1, 0)
mean(poly.deg5.predictions == df$Label)


poly.deg10.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'polynomial', degree = 10)
poly.deg10.predictions <- ifelse(predict(poly.deg10.svm.fit) > 0, 1, 0)
mean(poly.deg10.predictions == df$Label)


poly.deg12.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'polynomial', degree = 12)
poly.deg12.predictions <- ifelse(predict(poly.deg12.svm.fit) > 0, 1, 0)
mean(poly.deg12.predictions == df$Label)

df <- cbind(df, data.frame(Deg3SVM = poly.deg3.predictions, 
							Deg5SVM = poly.deg5.predictions, 
							Deg10SVM = poly.deg10.predictions, 
							Deg12SVM = poly.deg12.predictions))
							
predictions <- melt(df, id.vars = c('X', 'Y'))
ggplot(predictions, aes(x = X, y = Y, color = factor(value))) + geom_point() + facet_grid(variable ~ .)

#use cross validation to tune cost parameter for radial svm
df <- df[, c('X', 'Y', 'Label')]

set.seed(1)
k <- 5
prediction.accuracy <- data.frame()
folds <- cvFolds(nrow(df), K = k)


for (cst in c(1, 3, 5, 10, 12)){
	for (i in 1:k){
			train <- df[folds$subsets[folds$which != i], ]
			validation <- df[folds$subsets[folds$which == i], ]
	
			radial.svmfit <- svm(Label ~ X + Y, data = train, kernel = 'radial', cost = cst)
			
			prediction <- ifelse(predict(radial.svmfit, newdata = validation) > 0, 1, 0)
			
			accuracy <- mean(prediction == validation$Label)
			
			prediction.accuracy <- rbind(polynomial.prediction, data.frame(Degree = deg, Accuracy = accuracy))	
	}
}

aggregate( . ~ Degree, prediction.accuracy, mean) #based on the results, cost 12 seems to provide better accuracy

#build various radial svm models for different costs, compare their accuracies and plot the results
df <- df[, c('X', 'Y', 'Label')]

cost1.svmfit <- svm(Label ~ X + Y, data = df, kernel = 'radial', cost = 1)
cost1.prediction <- ifelse(predict(cost1.svmfit) > 0, 1, 0)
mean(cost1.prediction == df$Label)

cost2.svmfit <- svm(Label ~ X + Y, data = df, kernel = 'radial', cost = 2)
cost2.prediction <- ifelse(predict(cost2.svmfit) > 0, 1, 0)
mean(cost2.prediction == df$Label)

cost3.svmfit <- svm(Label ~ X + Y, data = df, kernel = 'radial', cost = 3)
cost3.prediction <- ifelse(predict(cost3.svmfit) > 0, 1, 0)
mean(cost3.prediction == df$Label)

cost4.svmfit <- svm(Label ~ X + Y, data = df, kernel = 'radial', cost = 4)
cost4.prediction <- ifelse(predict(cost4.svmfit) > 0, 1, 0)
mean(cost4.prediction == df$Label)

df <- cbind(df, data.frame(Cost1SVM = cost1.prediction, 
							Cost2SVM = cost2.prediction, 
							Cost3SVM = cost3.prediction, 
							Cost4SVM = cost4.prediction))
							
predictions <- melt(df, id.vars = c('X', 'Y'))
ggplot(predictions, aes(x = X, y = Y, color = as.factor(value))) + geom_point() + facet_grid(variable ~ .)


#use cross validation to tune gamma parameter for sigmoid svm
df <- df[, c('X', 'Y', 'Label')]

set.seed(1)
k <- 5
prediction.accuracy <- data.frame()
folds <- cvFolds(nrow(df), K = k)


for (gam in c(1, 3, 5, 10, 12)){
	for (i in 1:k){
			train <- df[folds$subsets[folds$which != i], ]
			validation <- df[folds$subsets[folds$which == i], ]
	
			sigmoid.svmfit <- svm(Label ~ X + Y, data = train, kernel = 'sigmoid', gamma = gam)
			
			prediction <- ifelse(predict(sigmoid.svmfit, newdata = validation) > 0, 1, 0)
			
			accuracy <- mean(prediction == validation$Label)
			
			prediction.accuracy <- rbind(polynomial.prediction, data.frame(Degree = deg, Accuracy = accuracy))	
	}
}

aggregate( . ~ Degree, prediction.accuracy, mean) #based on the results, gamma 10 seems to provide better accuracy


#build various sigmoid svm models for different gammas, compare their accuracies and plot the results
df <- df[, c('X', 'Y', 'Label')]

gamma1.svmfit <- svm(Label ~ X + Y, data = df, kernel = 'sigmoid', gamma = 1)
gamma1.prediction <- ifelse(predict(gamma1.svmfit) > 0, 1, 0)
mean(gamma1.prediction == df$Label)

gamma2.svmfit <- svm(Label ~ X + Y, data = df, kernel = 'sigmoid', gamma = 2)
gamma2.prediction <- ifelse(predict(gamma2.svmfit) > 0, 1, 0)
mean(gamma2.prediction == df$Label)

gamma3.svmfit <- svm(Label ~ X + Y, data = df, kernel = 'sigmoid', gamma = 3)
gamma3.prediction <- ifelse(predict(gamma3.svmfit) > 0, 1, 0)
mean(gamma3.prediction == df$Label)

gamma4.svmfit <- svm(Label ~ X + Y, data = df, kernel = 'sigmoid', gamma = 4)
gamma4.prediction <- ifelse(predict(gamma4.svmfit) > 0, 1, 0)
mean(gamma4.prediction == df$Label)

df <- cbind(df, data.frame(Gamma1SVM = gamma1.prediction, 
							Gamma2SVM = gamma2.prediction, 
							Gamma3SVM = gamma3.prediction, 
							Gamma4SVM = gamma4.prediction))
							
predictions <- melt(df, id.vars = c('X', 'Y'))
ggplot(predictions, aes(x = X, y = Y, color = as.factor(value))) + geom_point() + facet_grid(variable ~ .)



#####################Comparing Algorithms######################
