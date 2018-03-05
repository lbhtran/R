##################### FINAL STATISTICS ASSIGNMENT #######################

########## Load all the required packages ############

library("car")
library("leaps")

########## Load data frame ########

movies <- read.csv("data/movies1.csv") ## import dataframe
movies # display the data

movies[movies$Film =="Paranormal Activity",]

movies[movies[,12]=="760.5",]

########## DATA EXPORATION ########

### NUMERICAL VARIABLES ###

### Do a quick summary of all the numerical data

summary(movies[,c(4,5,8,9,10,11,12,13,14)])
for (i in c(4,5,8,9,10,11,12,13,14)) 
{a=cbind(a, sd(movies[,i], na.rm=T))}
a

### Plot all the continuous variables
pairs(movies[,c(4,5,8,9,10,11,12,13,14)])

### Find the correlation coefficient of all the numerical variables
cor(movies[,c(4,5,8,9,10,11,12,13,14)], use="complete")

### Rating Scores: RottenTomatoes and AudienceScores
summary(movies[,c(4,5)])
par(mfrow=c(1,2))
#hist(movies[,4])
boxplot(movies[,c(4,5)], horizontal=T, xlab="Ratings (out of 100)", ylim=c(0,100), main="Boxplot of Critics and Public Ratings")
plot(movies[,c(4,5)], main="Scatterplot of RottenTomatoes vs. AudienceScores")	
### rating seems to be fine, no need for transformation

sd(movies[,4], na.rm=T) ## Find the standard deviation of RottenTomatoes
sd(movies[,5], na.rm=T)	## Find the standard deviation of AudienceScores

### look at all variables that measured in million USD
### recalculate WorldwideGross
movies[,14] <- movies[,12] + movies[,13]
### Budget, OpeningWeekend, DomesticGross, ForeignGross and WorldwideGross
summary(movies[,c(8,11,12,13,14)]) ## Data summary
sapply(movies[,c(8,11,12,13,14)],sd,na.rm=T) ## Standard deviation

### Boxplot
par(mar=c(4,10,2,2))
boxplot(movies[,c(8,11,12,13,14)], horizontal=T, las=1, xlab="Millions USD", main="Boxplot of OpeningWeekend, Budget and Gross Profits")	
### Note: from the plot, we may have to log transform all of them

par(mar=c(4,10,2,2))
boxplot(log10(movies[,c(8,11,14)]), horizontal=T, las=1, xlab="Millions USD", main="Boxplot of log-transformed OpeningWeekend, Budget and WorldwideGross")
### Note: there seems to be still too many outliers

boxplot(movies[,c(8,11,12,13,14)], horizontal=T, las=1, xlab="Millions USD", main="Boxplot of transformed OpeningWeekend, Budget and Gross Profits")


### CinemasOpenWeek and AverageOpenWeek
summary(movies[,c(9,10)])
sapply(movies[,c(9,10)], sd, na.rm=T)

### Boxplot for CinemasOpenWeek and AverageOpenWeek
par(mfrow=c(2,2))
boxplot(movies[,9], horizontal=T, xlab="Number of cinemas", ylab="CinemasOpenWeek", main="Boxplot of CinemasOpenWeek")	

boxplot(movies[,10], horizontal=T, xlab="USD", ylab="AverageOpenWeek", main="Boxplot of AverageOpenWeek")	
### AverageOpenWeek needs to be transformed along with the other variables in millions USD (Note: this one is only in USD)

### CinemasOpenWeek seems to be fine
hist(movies[,9], xlab="Number of cinemas", ylab="Frequency", main="Histogram of CinemasOpenWeek")

boxplot(log10(movies[,10]), horizontal=T, ylab="log(AverageOpenWeek)", main="Boxplot of AverageOpenWeek after transformation")	
### transform AverageOpenWeek

### log-transform the variables necessary

movies1 <- movies ### add a new dataframe

movies1[,8]  <- log10(movies[,8])	  	## Budget
movies1[,10] <- log10(movies[,10])	## AverageOpenWeek
movies1[,11] <- log10(movies[,11])	## OpeningWeekend
#movies1[,12] <- log10(movies[,12])	## DomesticGross
#movies1[,13] <- log10(movies[,13])	## ForeignGross
movies1[,14] <- log10(movies[,14])	## WorldwideGross

### re-Plot all the continuous variables
pairs(movies1[,c(4,5,8,9,10,11,12,13,14)])
boxplot(movies1[,c(8,10,11,12,13,14)])

### CATEGORICAL VARIABLES ###

summary(movies1[,c(1,3,6,7)])
table(movies1[,c(1,3,6,7)])

### Year ########
is.factor(movies1[,1])	## Check if Year is interpret as a factor, no
movies1[,1] <- as.factor(movies1[,1])	## make Year a factor

summary(movies1[,1])
plot(movies1[,1], ylab="Number of movies", xlab="Year", ylim=c(0,160), main="Barplot of Number of movies in each Year")
text(summary(movies1[,1]))

### Studios

### 
exmovies <- read.csv("data/movies.csv") ## import dataframe
length(summary(exmovies[,3]))

length(summary(movies[,3]))
summary(movies1[,3])
count(summary(movies1[,3]))		## load from plyr package
summary(movies1[,3], maxsum=12)
par(mar=c(4,10,2,2))
barplot(rev(summary(movies1[,3], maxsum=12)), xlim=c(0,100), las=1, horiz=TRUE)
barplot(rev(summary(movies1[,3])), xlim=c(0,100), las=1, horiz=TRUE)

#Too many category in Studio, use 'car' to collapse some into 

require("car")    
movies1[,3] <- recode(movies1[,3], "c('Warner Bros.','Disney','Universal','Columbia','20th Century Fox','Paramount')='Major'")

summary(movies1[,3])

movies1[,3] <- recode(movies1[,3], "c('Aardman Animations', 'Buena Vista', 'CBS Films', 'Crest','DreamWorks','DreamWorks Animation','Focus','Happy Madison Productions','Highlight Communications', 'Legendary Pictures', 'Liberty Starz', 'Lionsgate', 'MGM','Miramax', 'Morgan Creek Productions', 'New Line Cinema', 'Pixar', 'Regency Enterprises', 'Relativity Media', 'Reliance Entertainment', 'Sony', 'Spyglass Entertainment', 'Summit Entertainment', 'Vertigo Entertainment','Village Roadshow Pictures','Virgin', 'Weinstein Company')='Others'")

summary(movies1[,3])

par(mfrow=c(1,2))
## Studio plot
barplot(summary(movies1[,3]), ylim=c(0,200), las=1, xlab="Type of Studios", ylab="Number of Movies", main="Number of Movies for each type of studios")
## Year vs Studio Type
plot(movies1[,1], movies1[,3], las=2, xlab="Year", ylab="Type of Studios", main="Year vs Studio Types")


### Story
length(summary(movies1[,6]))
summary(movies1[,6])
par(mar=c(4,10,2,2))
barplot(rev(summary(movies1[,6])), horiz=TRUE, las=1, xlab="Number of Movies", xlim=c(0,70), main="Number of Movies for each Plot Type")
storyplot <- barplot(rev(summary(movies1[,6])), horiz=TRUE, las=1, xlab="Number of Movies", xlim=c(0,70), main="Number of Movies for each Plot Type")
text(rev(summary(movies1[,6]))+1.5, storyplot, labels=rev(summary(movies1[,6])))



### Genre
length(summary(movies1[,7]))
summary(movies1[,7])
par(mar=c(4,10,2,2))
genreplot <- barplot(rev(summary(movies1[,7])),xlim=c(0,120), horiz=TRUE, las=1, xlab="Number of Movies", main="Number of Movies for each Genre")
barplot(rev(summary(movies1[,7])),xlim=c(0,120), horiz=TRUE, las=1, xlab="Number of Movies", main="Number of Movies for each Genre")

text(rev(summary(movies1[,7]))+2.5, genreplot, labels=rev(summary(movies1[,7])))

### Oscars and Bafta

summary(movies1[,c(15,16)])
## Top ten Worldwide Gross movies does not receive any award, may not contribute anything to the analysis
## furthermore, the analysis asks for variables that can be used to predict the revenue of a movie shortly after
## releasing, Oscars and Bafta are not appropriate to use as the awards are usually given the following year.
## However, we may do an analysis to predict whether a movie will receive any award


### Plots against Worldwide Gross
par(mfrow=c(2,2))

## Year vs. WG
plot(movies1[,c(1,14)], ylab="log(WorldwideGross)")

## Studios vs. WG
plot(movies1[,c(3,14)], ylab="log(WorldwideGross)")

## Story vs. WG
plot(movies1[,c(6,14)], ylab="log(WorldwideGross)")

## Genre vs. WG
par(mar=c(10,4,2,2))
plot(movies1[,c(7,14)], ylab="log(WorldwideGross)", las=3, xlab=NULL)

 

######## Treating the missing values ########
## iterate all the rows with missing values
namovies <- movies[!complete.cases(movies),]
length(namovies[,1]) ## there are 37 cases with missing values
namovies
namovies[,1] <- as.factor(namovies[,1])
summary(namovies[,c(1,3,6,7)])
summary(namovies)


## remove all the cases with missing values
movies2 <- na.omit(movies1)
length(movies2[,1]) ## there are 377 movies left
summary(movies2[,c(1,3,6,7)])



######## LINEAR REGRESSION ##########


### Revenue - No Categorical Variables
revenue.lm1 <- lm(WorldwideGross ~ RottenTomatoes + AudienceScore + Budget + CinemasOpenWeek + AverageOpenWeek + OpeningWeekend, data=movies2)
summary(revenue.lm1)
vif(revenue.lm1)

revenue.lm2 <- step(revenue.lm1)
summary(revenue.lm2)

## Residual Plots
par(mfrow=c(2,2))
plot(density(revenue.lm2$residuals))
plot(revenue.lm2, which=c(1,2,5))

movies[285,]

par(las=2,mar=c(12,4,2,2))
scatterplot(residuals(revenue.lm2)~movies2[,3], xlab=NULL)
abline(0,0)

scatterplot(residuals(revenue.lm2)~movies2[,5])
abline(0,0)

## Revenue - with Year and Studio
revenue.lm3 <- lm(WorldwideGross ~ Year + Studio + RottenTomatoes + AudienceScore + Budget + CinemasOpenWeek + AverageOpenWeek + OpeningWeekend, data=movies2)
summary(revenue.lm3)



### Profit - No Categorical Variables
profit.lm1 <- lm(WorldwideGross - Budget ~ RottenTomatoes + AudienceScore + Budget + CinemasOpenWeek + AverageOpenWeek + OpeningWeekend, data=movies2)
summary(profit.lm1)

## Residual Plots
par(mfrow=c(2,2))
plot(density(profit.lm1$residuals))
plot(profit.lm1, which=c(1,2,5))

#### Outliers - Paranormal Activity ####
#### The solution to this outlier is to remove it

### Revenue - No Categorical Variables
revenue.lm1 <- lm(WorldwideGross ~ RottenTomatoes + AudienceScore + Budget + CinemasOpenWeek + AverageOpenWeek + OpeningWeekend, data=movies1[-223,])
summary(revenue.lm1)

## Residual Plots
par(mfrow=c(2,2))
plot(density(revenue.lm1$residuals))
plot(revenue.lm1, which=c(1,2,5))

### best subset
movies2[,1]<- as.factor(movies2[,1])
YearDummy <- model.matrix( ~ Year - 1, data = movies2)

YearDummy

require("car")
require("leaps")


attach(movies2)
pred.matrix <- cbind(movies2[,c(4,5)], YearDummy)
pred.matrix <- as.matrix(pred.matrix)
is.matrix(pred.matrix)
best.year <- leaps(pred.matrix, movies2[,14], nbest=3)



######### LOGISTIC REGRESSION ##########

## create a new binary factor

nonUS <- ifelse(movies1$DomesticGross < movies1$ForeignGross, 1, 0)
movies3 <- cbind(movies1,nonUS)

movies3 <- movies3[,-17]
movies3

is.factor(nonUS)
nonUS <- as.factor(nonUS)

plot(movies3[,c(8,17)])

attach(movies3) 
 
revenue.glm1 <- glm(nonUS ~ RottenTomatoes + AudienceScore + Budget + CinemasOpenWeek + AverageOpenWeek + OpeningWeekend, family = "binomial", data=movies3, maxit=1000)

summary(revenue.glm1)



### 