########### Final Statistics Assignment ###########

movies <- read.csv("data/movies1.csv") ## import dataframe
movies # display the data

names(movies) ## this line is just to shorten the code when modelling

length(movies$Year) ### check for number of observations

### Get a summary for all the values
summary(movies)

### Export to Excel Spreadsheet
library(xlsx)
write.xlsx(summary(movies), "e:/R/Movies-summary.xlsx") 

### CONTINUOUS VARIABLES ###

### Plot all the continuous variables
pairs(movies[,c(4,5,8,9,10,11,12,13,14)])

boxplot(movies[,c(8,11,12,13,14)])	### look at all variables that measured in million USD
### Note: from the plot, we may have to log transform all of them

boxplot(log(movies[,c(8,11,12,13,14)]))
### Note: there seems to be still too many outliers

boxplot(movies[,c(4,5)])	### rating seems to be fine, no need for transformation

boxplot(movies[,9])	### CinemasOpenWeek seems to be fine

boxplot(movies[,10])	### AverageOpenWeek needs to be transformed along with the other variables in millions USD (Note: this one is only in USD)
boxplot(log(movies[,10]))	### transform AverageOpenWeek

### log-transform the variables

logBudget <- log(movies[,8])	## Budget
logAOW <- log(movies[,10])	## AverageOpenWeek
logOW	<- log(movies[,11])	## OpeningWeekend
logDG <- log(movies[,12])	## DomesticGross
logFG <- log(movies[,13])	## ForeignGross
logWG <- log(movies[,14])	## WorldwideGross

### add these new variables to the movies data frame
movies1 <-cbind(movies, logBudget,logAOW,logOW,logDG,logFG,logWG)


### do another matrix plots with with all variables after log-transformation
names(movies1)
pairs(movies1[,c(4,5,17,9,18,19,20,21,22)])

is.factor(movies1[,1])	## Check if Year is interpret as a factor, no
movies1[,1] <- as.factor(movies1[,1])	## make Year a factor

summary(movies1[,1])

### CATEGORICAL VARIABLES ###

summary(movies1[,c(1,3,6,7)])



### Explore the categorical variables
ftable(table(movies[,1], movies[,3]))
movies.factors <- table(movies[,c(1,3,6,7)])
ftable(movies.factors)

summary(movies.factors)