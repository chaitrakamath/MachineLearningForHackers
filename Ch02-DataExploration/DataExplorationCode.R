getwd()
setwd('/Users/admin/Documents/GitHubCode/MLForHackersCode/Ch02-DataExploration')
getwd()

##Numeric summaries
heights.weights <- read.csv('01_heights_weights_genders.csv')
summary(heights.weights$Height)

##Means, Medians and Modes

#Your own code to compute mean and median
my.mean <- function(x){
	return (sum(x, na.rm = TRUE) / length(x))
}

my.median <- function(x){
	x_sorted <- sort(x)
	if(length(x) %% 2 == 0)
		{
		 medianIndex <- c(length(x) / 2, length(x) / 2 + 1)
		 return (mean(x_sorted[medianIndex]))
		}
	else 
		{
		medianIndex <- ceiling(length(x) / 2)
		return (x_sorted[medianIndex])
		}
}

#How mean and median work on data with even number of elements vs odd number of elements

vec1 <- c(0, 100)
print (vec1)
mean(vec1)
median(vec1)

vec2 <- c(0, 0, 100)
print (vec2)
mean(vec2)
median(vec2)

#Testing self defined functions for mean and median
my.mean(heights.weights$Height)
mean(heights.weights$Height) #built in function of R

my.median(heights.weights$Height)
median(heights.weights$Height) #built in function of R


##Quantiles
#How to find min, max and range of a numeric column
min(heights.weights$Height) #minimum of heights

max(heights.weights$Height) #maximum of heights

c(min(heights.weights$Height), max(heights.weights$Height)) #range of heights

range(heights.weights$Height) #range using R's built in function

##How to find data below N% of your data set
quantile(heights.weights$Height) 
quantile(heights.weights$Height, probs = seq(0, 1, by = 0.20))


##Standard Deviations and Variances
#Self-defined function to compute variance
my.var <- function(x){
	m <- mean(x)
	num <- sum((x-m) ^ 2)
	den <- length(x) - 1
	return (num / den) 
}

my.var(heights.weights$Height) - var(heights.weights$Height) #test our function against R's built in function for variance

#Self-defined function to compute sd
my.sd <- function(x){
	variance <- my.var(x)
	return (sqrt(variance))
}

my.sd(heights.weights$Height) - sd(heights.weights$Height) #test our function against R's built in function for sd


##Exploratory Data Visualization
library(ggplot2)

#Viz1: Histograms
ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 1)
ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 5)

#Viz2: Kernel Density plots
ggplot(heights.weights, aes(x = Height)) + geom_density()
ggplot(heights.weights, aes (x = Height, fill = Gender)) + geom_density()
ggplot(heights.weights, aes (x = Weight, fill = Gender)) + geom_density()

#Viz3: Kernel Density plots split by facets
ggplot(heights.weights, aes(x = Height, fill = Gender)) + geom_density() + facet_grid(Gender ~ .)

#Viz4: Normal vs cauchy plots
set.seed(1)
normal.values <- rnorm(250, mean = 0, sd = 1)
cauchy.values <- rcauchy(250, location = 0, scale = 1)

range(normal.values)
range(cauchy.values)

ggplot(data = data.frame(X = normal.values), aes(x = X)) + geom_density()
ggplot(data = data.frame(X = cauchy.values), aes(x = X)) + geom_density()

#Viz4: Gamma distribution
gamma.values <- rgamma(10000,shape = 1, rate = 0.001)
ggplot(data = data.frame(X = gamma.values), aes(x = X)) + geom_density()

##Visualizing relationships between columns
#Viz1: Scatter plots
ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point()
ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point() + geom_smooth()
ggplot(heights.weights[1:20,], aes(x = Height, y = Weight)) + geom_point() + geom_smooth()
ggplot(heights.weights[1:200,], aes(x = Height, y = Weight)) + geom_point() + geom_smooth()
ggplot(heights.weights[1:2000,], aes(x = Height, y = Weight)) + geom_point() + geom_smooth()

ggplot(heights.weights, aes(x = Height, y = Weight, color = Gender)) + geom_point() + geom_smooth() + facet_grid(Gender ~ .)