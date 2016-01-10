#File Name: ufo.R
#Date: Oct 12, 2015
#Purpose: Reading through first chapter of ML for hackers and writing code 
#Data Used: Download ufo_awesome.tsv file from the following website:
#https://github.com/johnmyleswhite/ML_for_Hackers/tree/master/01-Introduction/data/ufo
#Packages Used: ggplot2, plyr, scales, stringr

#LOADING LIBRARIES AND DATA
#load required packages
library(ggplot2)
library(plyr)
library(reshape)
library(stringr)

#read data
ufo <- read.delim(file = '/Users/admin/Documents/GitHubCode/MLForHackersCode/Ch01/data/ufo_awesome.tsv', header = FALSE, sep = '\t', stringsAsFactors = FALSE, na.strings = "")

#view some data
str(ufo)
head(ufo)
tail(ufo)

#since there are no column names, create them
columnNames <- c('DateOccured', 'DateReported', 'Location', 'ShortDescription', 'Duration', 'LongDescription')
names(ufo) <- columnNames



#CONVERTING DATE STRINGS AND DEALING WITH MALFORMED DATA
#clean out date columns to remove non-date values
head(ufo[which(nchar(ufo$DateOccured) != 8 | nchar(ufo$DateReported) != 8) , c('DateOccured', 'DateReported')])
ufo_cleaned <- ufo[which(nchar(ufo$DateOccured) == 8 | nchar(ufo$DateReported) == 8), ]

#Now that we have cleaned up date columns, convert those character strings to date format
ufo_cleaned$DateOccured <- as.Date(ufo_cleaned$DateOccured, format = '%Y%m%d')
ufo_cleaned$DateReported <- as.Date(ufo_cleaned$DateReported, format = '%Y%m%d')


#ORGANIZING LOCATION DATA
#Split the location to city and state columns, clean them if necessary and run a check to make sure that all the locations belong to USA
locationList <- strsplit(ufo_cleaned$Location, split = ',')
ufo_cleaned$City <- sapply(locationList, function(x) x[1])
ufo_cleaned$State <- sapply(locationList, function(x) x[2])

#remove leading and trailing whitespaces city and state
ufo_cleaned$State <- str_trim(ufo_cleaned$State, side = 'both') 
ufo_cleaned$City <- str_trim(ufo_cleaned$City, side = 'both')

#if length of State is greater than 2, it is likely not a valid state. So, all these values will be replaced by NA for state and city columns
ufo_cleaned$State[nchar(ufo_cleaned$State) > 2] <- NA 
ufo_cleaned$City[nchar(ufo_cleaned$State) > 2] <- NA

#inspect unique values of state column
sort(unique(ufo_cleaned$State[!is.na(ufo_cleaned$State)]))



#DEALING WITH DATA OUT OF SCOPE
#Since state column has mixed case, convert all states to lower-case
ufo_cleaned$State[!is.na(ufo_cleaned$State)] <- tolower(ufo_cleaned$State[!is.na(ufo_cleaned$State)])

#We see that there are some states that are not a part of US. We would remove these and NA rows as we are interested in analyzing ufo sightings in USA only
ufo_usa <- subset(ufo_cleaned, ufo_cleaned$State %in% tolower(state.abb))



#AGGREGATING AND ORGANIZING DATA
#analyze and plot range of Date Occurred column 
summary(ufo_usa$DateOccured)
quick.hist <- ggplot(data = ufo_usa, aes(x = DateOccured)) + geom_histogram()+scale_x_date(breaks = '50 years')
ggsave(filename = '/Users/admin/Documents/GitHubCode/MLForHackersCode/Ch01/images/quick.hist.png', plot = quick.hist,height = 6,width = 20)
#since a lot of data is concentrated between 1990 and 2010, we would subset data to include sightings that occurred after 1990
ufo_usa <- subset(ufo_usa, ufo_usa$DateOccured > '1990-01-01')
