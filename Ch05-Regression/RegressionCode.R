getwd()
setwd('/Users/admin/Documents/GitHubCode/MLForHackersCode/Ch05-Regression/data')
getwd()

library(ggplot2)


#################Baseline Model##########################
#Read and plot data
ages <- read.csv('longevity.csv')
ggplot(ages, aes(x = AgeAtDeath, fill = factor(Smokes))) + geom_density() + facet_grid(Smokes ~ .)

#Create a baseline model by using mean of AgeAtDeath as predicted value and compute error
prediction <- round(mean(ages$AgeAtDeath))
error <- sum((ages$AgeAtDeath - prediction) ^ 2) / nrow(ages)

#Create baseline model by using different values as predicted values and plot the error output
rowNum <- 1
error.df <- data.frame(PredictedValue = as.numeric(), Error = as.numeric())
for (i in seq(63, 83)){
	prediction <- i
	error.df[rowNum, ] <- c(i, sum((ages$AgeAtDeath - prediction) ^ 2) / nrow(ages))
	rowNum <- rowNum + 1
}
ggplot(error.df, aes(x = PredictedValue, y = Error)) + geom_point() + geom_line()

#################Regression using dummy variables#################
#Predict mean of smoking vs non smoking group as age of death 
prediction.without.grouping <- mean(ages$AgeAtDeath)
smoking.mean <- mean(ages$AgeAtDeath[ages$Smokes == 1])
nonsmoking.mean <- mean(ages$AgeAtDeath[ages$Smokes == 0])

#add predicted values to ages data frame
ages$NewPrediction <- NA
ages$NewPrediction <- ifelse(ages$Smokes == 0, nonsmoking.mean, smoking.mean)

#Compute RMSE based on prediction with and without grouping data
error.without.grouping <- sqrt(mean((ages$AgeAtDeath - prediction.without.grouping) ^ 2))

error.with.grouping <- sqrt(mean((ages$AgeAtDeath - ages$NewPrediction) ^ 2))


#################Linear Regression in a nutshell##################
#plot weight vs height dataset
heights.weights <- read.csv('01_heights_weights_genders.csv')
ggplot(heights.weights, aes(x = Weight, y = Height)) + geom_point() + geom_smooth(method = 'lm')

#fit a linear model to predict weight based on height
lmModel <- lm(Weight ~ Height, data = heights.weights)
coef(lmModel) #to get intercept and slope of lmModel
intercept <- coef(lmModel)[1]
slope <- coef(lmModel)[2]

#predicted.weight = intercept + slope * observed.height
#predicted.weight = - 350.737192 + 7.717288 * observed.height

#predict weights using the linear model we built
predicted.weights = predict(lmModel)

#compute residuals
lmResidual <- heights.weights$Weight - predicted.weights
residuals(lmModel) #alternate to above line of code

#residuals plot
plot(lmModel, which = 1)

#example of linear model on non-linear data
x <- 1:10
y <- x ^ 2
fitted.regression <- lm(y ~ x)
plot(fitted.regression, which = 1)
#compute sum of squared errors (SSE)
errors <- residuals(fitted.regression)
squared.error <- errors ^ 2
sum(squared.error)
#compute mean of squared errors (MSE)
errors <- residuals(fitted.regression)
squared.error <- errors ^ 2
mse <= mean(squared.error)
#compute rmse
errors <- residuals(fitted.regression)
squared.error <- errors ^ 2
mse <- mean(squared.error)
sqrt(mse)
#compute R-squared
mean.rmse <- sqrt(mean((heights.weights$Weight - mean(heights.weights$Weight)) ^ 2))
model.rmse <- sqrt(mse) #computed from above
r2 <- 1 - (model.rmse / mean.rmse)


#################Predicting Web Traffic##########################
topSites <- read.csv('top_1000_sites.tsv', sep = '\t', stringsAsFactors = FALSE)

#scatter plot of numerical variables
ggplot(topSites, aes(x = UniqueVisitors, y = PageViews)) + geom_point() #do not see any linear trend. Hence, try density plot of output variable
ggplot(topSites, aes(x = PageViews)) + geom_density() #extremely skewed. View log transformed data plot
ggplot(topSites, aes(x = log(PageViews))) + geom_density()
#Since the log transformed plot looks better, we will log transform both: feature and output variable
ggplot(topSites, aes(x = log(UniqueVisitors), y = log(PageViews))) + geom_point() + geom_smooth(method = 'lm', se = FALSE)

#create a linear model to predict pageviews based on number of unique visitors
lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors), data = topSites)
summary(lm.fit)

#build new linear fit model using by including predictor variables too
lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors) + HasAdvertising + InEnglish, data = topSites)
summary(lm.fit)


#################Defining Correlation##########################
x <- 1:10
y <- x ^ 2

ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)

cor(x, y)
coef(lm(scale(y) ~ scale(x)))