########### Final Statistics Assignment ###########

movies <- read.csv("data/movies2.csv") ## import dataframe
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
attach(movies1)

pairs(movies1[,c(4,5,17,9,18,19,20,21,22)])



summary(movies1[,1])

### CATEGORICAL VARIABLES ###

summary(movies1[,c(1,3,6,7)])
table(movies1[,c(1,3,6,7)])

### Year

is.factor(movies1[,1])	## Check if Year is interpret as a factor, no
movies1[,1] <- as.factor(movies1[,1])	## make Year a factor

summary(movies1[,1])
par(mar=c(4,10,2,2))
barplot(summary(movies1[,1]))

### Studios

summary(movies1[,3])
summary(movies1[,3], maxsum=12)
par(mar=c(4,10,2,2))
barplot(rev(summary(movies1[,3], maxsum=12)), xlim=c(0,100), las=1, horiz=TRUE)


### Too many category, use 'car' to collapse some

require("car")    
movies1[,3] <- recode(movies1[,3], "c('CBS Films','Columbia','DreamWorks','Focus','Happy Madison Productions','MGM','Miramax','New Line Cinema','Reliance Entertainment','Spyglass Entertainment','Vertigo Entertainment','Village Roadshow Pictures','Virgin')='Others'")

summary(movies1[,3])


### Story

summary(movies1[,6])
par(mar=c(4,10,2,2))
barplot(rev(summary(movies1[,6])), horiz=TRUE, las=1)

### Genre

summary(movies1[,7])
par(mar=c(4,10,2,2))
barplot(rev(summary(movies1[,7])),xlim=c(0,120), horiz=TRUE, las=1)

### Oscars and Bafta

summary(movies1[,c(15,16)])
## Top ten Worldwide Gross movies does not receive any award, may not contribute anything to the analysis
## furthermore, the analysis asks for variables that can be used to predict the revenue of a movie shortly after
## releasing, Oscars and Bafta are not appropriate to use as the awards are usually given the following year.
## However, we may do an analysis to predict whether a movie will receive any award

### 2-way tables

## Years and Studios
### Explore the categorical variables
ftable(table(movies[,1], movies[,3]))
movies.factors <- table(movies1[,c(1,3)])
ftable(movies.factors)
plot(movies.factors)

summary(movies.factors)

### Plots against Worldwide Gross

par(mfrow=c(2,2))

## Year vs. WG
plot(movies1[,c(1,22)], ylab="log(WorldwideGross)")

## Studios vs. WG
plot(movies1[,c(3,22)], ylab="log(WorldwideGross)")

## Story vs. WG
plot(movies1[,c(6,22)], ylab="log(WorldwideGross)")

## Genre vs. WG
par(mar=c(10,4,2,2))
plot(movies1[,c(7,22)], ylab="log(WorldwideGross)", las=3, xlab=NULL)



######## LINEAR REGRESSION ##########

### Revenue - All variables
attach(movies1)
revenue.lm1 <- lm(logWG ~ Year + Studio + RottenTomatoes + AudienceScore + Story + Genre + logBudget 
+ CinemasOpenWeek + logAOW + logOW, data=movies1)
summary(revenue.lm1)

## Residual Plots
par(mfrow=c(2,2))
plot(density(revenue.lm1$residuals))
plot(revenue.lm1, which=c(1,2,5))

### Revenue - No Story
revenue.lm2 <- lm(logWG ~ Year + Studio + RottenTomatoes + AudienceScore + Genre + logBudget 
+ CinemasOpenWeek + logAOW + logOW, data=movies1)
summary(revenue.lm2)

## Residual Plots
par(mfrow=c(2,2))
plot(density(revenue.lm2$residuals))
plot(revenue.lm2, which=c(1,2,5))

### Revenue - No Categorical Variables
revenue.lm3 <- lm(logWG ~ RottenTomatoes + AudienceScore + logBudget 
+ CinemasOpenWeek + logAOW + logOW, data=movies1)
summary(revenue.lm3)

## Residual Plots
par(mfrow=c(2,2))
plot(density(revenue.lm3$residuals))
plot(revenue.lm3, which=c(1,2,5))





### Profit - no Story
profit.lm1 <- lm(logWG-logBudget ~ Year + Studio + RottenTomatoes + AudienceScore + Genre + CinemasOpenWeek 
+ logAOW + logOW, data=movies1)
summary(profit.lm1)

## Residual Plots
par(mfrow=c(2,2))
plot(density(profit.lm1$residuals))
plot(profit.lm1, which=c(1,2,5))

movies1[223,]







### Try best subsets method

require("leaps")
subsets.revenue <- regsubsets(logWG ~ Year + Studio + logBudget + RottenTomatoes + AudienceScore + Genre + logOW, data=movies1, nbest =3) 

par(mfrow=c(1,2))
plot(subsets.revenue, scale = "adjr2")
plot(subsets.revenue, scale = "Cp")

summary(subsets.revenue)

names(subsets.revenue)

library(car)

## Adjusted R2
res.legend <- subsets(subsets.revenue, statistic="adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")
## Mallow Cp
res.legend <- subsets(subsets.revenue, statistic="cp", legend = F, min.size = 1, main = "Mallow Cp", ylim=c(-5,5), xlim=c(0,15))
abline(a = 1, b = 1, lty = 2)

res.legend

contrasts(movies1$Year) <- contr.treatment(3)
contrasts(movies1$Studio) <- contr.treatment(12)
contrasts(movies1$Genre) <- contr.treatment(13)

attach(movies1)
Year.cat <- factor(movies1$Year, levels = 1:3)
movies1$Year
Studio.cat <- factor(movies1$Studio, levels = 1:12)
movies1$Studio
Story.cat <- factor(movies1$Story, levels = 1:22)
movies1$Story
Genre.cat <- factor(movies1$Genre, levels = 1:13)
movies1$Genre


#### Apply leaps to find best subsets
subsets.revenue1 <- regsubsets(logWG ~ Year + Studio + logBudget + RottenTomatoes + AudienceScore + Genre + logOW, data=movies1, nbest =3) 
par(mfrow=c(1,3))
plot(subsets.revenue1, scale = "adjr2")
plot(subsets.revenue1, scale = "Cp")
res.legend <- subsets(subsets.revenue1, statistic="cp", legend = F, min.size = 1, main = "Mallow Cp", ylim=c(-5,5), xlim=c(0,15))
abline(a = 1, b = 1, lty = 2)

summary.out <- summary(subsets.revenue1)
names(summary.out)
summary.out$cp
summary.out$which[11,]
summary.out$outmat

x <- as.matrix(movies1[,c(1,3,4,5,7,17,9,18,19)])
y <- movies1[,22]

leaps(x, y)

all <- lm(logWG ~ Year + Studio + RottenTomatoes + AudienceScore + Genre + logBudget + CinemasOpenWeek + logAOW + logOW, data=movies1)
revenue1 <- lm(logWG ~ Year + Studio + logBudget + RottenTomatoes + AudienceScore + Genre + logOW, data=movies1)

step.revenue <- step(revenue1)
summary(step.revenue)

step.all <- step(all)
summary(step.all)

## Residual Plots
par(mfrow=c(2,2))
plot(density(step.all$residuals))
plot(step.all, which=c(1,2,5))


revenue.lmbs1 <- lm(logWG ~ Studio + logBudget + AudienceScore + logOW, data=movies1)
summary(revenue.lmbs1)



###### Remove data point 223 which is Paranormal Activity
movies2 <- movies1[-223,]
all <- lm(logWG ~ Year + Studio + RottenTomatoes + AudienceScore + Genre + logBudget + CinemasOpenWeek + logAOW + logOW, data=movies2)
step.all <- step(all)
summary(step.all)

## Residual Plots
par(mfrow=c(2,2))
plot(density(step.all$residuals))
plot(step.all, which=c(1,2,5))

## less influence
## now check what is 332
movies2[332,]
movies2[162,]

## Check for VIF
vif(step.all)

revenue2 <- lm(logWG ~ Year + Studio + logBudget + RottenTomatoes + AudienceScore + Genre + logOW, data=movies2)
step.revenue <- step(revenue2)
summary(step.revenue)

## Residual Plots
par(mfrow=c(2,2))
plot(density(step.revenue$residuals))
plot(step.revenue, which=c(1,2,5))


revenue3 <- lm(logWG ~ Year + Studio + Year:Studio + logBudget + RottenTomatoes + AudienceScore + Genre + logOW, data=movies2)
step.revenue <- step(revenue3)
summary(step.revenue)

## Residual Plots
par(mfrow=c(2,2))
plot(density(step.revenue$residuals))
plot(step.revenue, which=c(1,2,5))






##################### MISSING VALUES ####################

movies3 <- na.omit(movies1)
length(movies3$Year)

all <- lm(logWG ~ Year + Studio + RottenTomatoes + AudienceScore + Genre + logBudget + CinemasOpenWeek + logOW, data=movies3)
step.all <- step(all)
summary(step.all)

## Residual Plots
par(mfrow=c(2,2))
plot(density(step.all$residuals))
plot(step.all, which=c(1,2,5))



















