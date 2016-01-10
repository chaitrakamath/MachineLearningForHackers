getwd()
setwd('/Users/admin/Documents/GitHubCode/MLForHackersCode/Ch08-PCA-BuildingMarketIndex')
getwd()

#load required libraries
library(lubridate)
library(reshape)
library(ggplot2)
#####################Unsupervised Learning########################
#load data set
prices <- read.csv('data/stock_prices.csv')
str(prices)

#convert data type of Date column from factor to Date
prices <- transform(prices, Date = ymd(Date))
str(prices)

#convert prices dataframe into a pivot table with dates on rows, stocks on columns and closing price as the value
data.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

#check for any missing values in the resulting matrix
data.stock.matrix[apply(data.stock.matrix, 1, function(y) any(is.na(y))),]

#remove those rows from dataframe and create matrix again
prices <- subset(prices, !(Date %in% c(ymd('2002-02-01', '2005-06-22'))))
data.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

#create correlation matrix of data.stock.matrix
correlationMat <- cor(data.stock.matrix[, 2:ncol(data.stock.matrix)])
correlations <- as.numeric(correlationMat)
ggplot(data.frame(Correlation = correlations), aes(x = Correlation, fill = 1)) + geom_density() + theme(legend.position = 'none')

#compute principal components of data
pca <- princomp(data.stock.matrix[, 2:ncol(data.stock.matrix)])
print (pca)

#try to get weights that first principal component assigns to each column within our dataset
principal.comp <- pca$loadings[, 1]
loadings <- as.numeric(principal.comp)
ggplot(data.frame(Loading = loadings), aes(x = Loading, fill = 1)) + geom_density() + theme(legend.position = 'none')

#predict pca on data
market.index <- predict(pca)[, 1]

#use DJI data to evaluate accuracy of pca prediction
dji.prices <- read.csv('data/DJI.csv')
str(dji.prices)
dji.prices <- transform(dji.prices, Date = ymd(Date))
str(dji.prices)

#subset DJI data set to keep the date range we are interested in
dji.prices <- subset(dji.prices, Date > ymd('2002-01-01'))
dji.prices <- subset(dji.prices, !(Date %in% c(ymd('2002-02-01', '2005-06-22'))))

#reverse closing prices and dates in dji.prices dataframe
dji <- rev(dji.prices$Close)
dates <- rev(dji.prices$Date)

#compare the results of DJI with values predicted by PCA
comparison <- data.frame(Date = dates, MarketIndex = market.index, DJI = dji)

ggplot(comparison, aes(x = market.index, y = DJI)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)

comparison <- transform(comparison, MarketIndex = -1 * MarketIndex)
ggplot(comparison, aes(x = MarketIndex, y = DJI)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)

#evaluate how well prediction tracks DJI over time
alt.comparison <- melt(comparison, id.vars = 'Date')
names(alt.comparison) <- c('Date', 'Index', 'Price')
ggplot(alt.comparison, aes(x = Date, y = Price, group = Index, color = Index)) + geom_point() + geom_line()

#fix the discrepancy in the results by scaling market index and dji prices
comparison$MarketIndex <- -scale(comparison$MarketIndex)
comparison$DJI <- scale(comparison$DJI)

alt.comparison <- melt(comparison, id.vars = 'Date')
names(alt.comparison) <- c('Date', 'Index', 'Price')

ggplot(alt.comparison, aes(x = Date, y = Price, group = Index, color = Index)) + geom_point() + geom_line()