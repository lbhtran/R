##################### FINAL STATISTICS ASSIGNMENT #######################

########## Load all the required packages ############

library("car")
library("leaps")

########## Load data frame ########

movies <- read.csv("data/movies1.csv") ## import dataframe
movies # display the data

movies[movies$Film =="Paranormal Activity",]

########## DATA EXPORATION ########

### NUMERICAL VARIABLES ###

### Do a quick summary of all the numerical data

summary(movies[,c(4,5,8,9,10,11,12,13,14)])

### Plot all the continuous variables
pairs(movies[,c(4,5,8,9,10,11,12,13,14)])

### Find the correlation coefficient of all the numerical variables
cor(movies[,c(4,5,8,9,10,11,12,13,14)], use="complete")

### Rating Scores: RottenTomatoes and AudienceScores
summary(movies[,c(4,5)])
par(mfrow=c(1,2))
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
boxplot(movies[,c(8,11,12,13,14)], horizontal=T, las=1, xlab="Millions USD", main="Boxplot of OpeningWeekend, Budget and Gross Revenues")	
### Note: from the plot, we may have to log transform all of them

par(mar=c(4,10,2,2))
boxplot(log(movies[,c(8,11,14)]), horizontal=T, las=1, xlab="Millions USD", main="Boxplot of log-transformed OpeningWeekend, Budget and WorldwideGross")
### Note: there seems to be still too many outliers



### CinemasOpenWeek and AverageOpenWeek
summary(movies[,c(9,10)])
sapply(movies[,c(9,10)], sd, na.rm=T)

### Boxplot for CinemasOpenWeek and AverageOpenWeek
par(mfrow=c(1,3))
boxplot(movies[,9], horizontal=T, xlab="Number of cinemas", ylab="CinemasOpenWeek", main="Boxplot of CinemasOpenWeek", cex=2)	

boxplot(movies[,10], horizontal=T, xlab="USD", ylab="AverageOpenWeek", main="Boxplot of AverageOpenWeek", cex=2)	
### AverageOpenWeek needs to be transformed along with the other variables in millions USD (Note: this one is only in USD)

boxplot(log(movies[,10]), horizontal=T, ylab="log(AverageOpenWeek)", main="Boxplot of AverageOpenWeek after transformation", cex=2)	
### transform AverageOpenWeek

### log-transform the variables necessary

movies1 <- movies ### add a new dataframe

movies1[,8]  <- log(movies[,8])	  	## Budget
movies1[,10] <- log(movies[,10])	## AverageOpenWeek 
movies1[,11] <- log(movies[,11])	## OpeningWeekend
movies1[,12] <- log(movies[,12])	## DomesticGross (not really necessary)
movies1[,13] <- log(movies[,13])	## ForeignGross (not really necessary)
movies1[,14] <- log(movies[,14])	## WorldwideGross

### re-Plot all the transformed variables

par(mfrow=c(1,2))
pairs(movies[,c(8,10,11,14)])
pairs(movies1[,c(8,10,11,14)])
boxplot(movies1[,c(8,10,11,12,13,14)])

### Find the correlation coefficient of all the numerical variables
cor(movies1[,c(4,5,8,9,10,11,12,13,14)], use="complete")





### CATEGORICAL VARIABLES ###

summary(movies1[,c(1,3,6,7)])
table(movies1[,c(1,3,6,7)])

### Year ########
is.factor(movies1[,1])	## Check if Year is interpret as a factor, no
movies1[,1] <- as.factor(movies1[,1])	## make Year a factor

summary(movies1[,1])
plot(movies1[,1], ylab="Number of movies", xlab="Year", ylim=c(0,160), main="Barplot of Number of movies in each Year")
yearplot <- plot(movies1[,1], ylab="Number of movies", xlab="Year", ylim=c(0,160), main="Barplot of Number of movies in each Year")

text(summary(movies1[,1]), yearplot, labels=summary(movies1[,1]))

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

movies1[,3] <- recode(movies1[,3], "c('Aardman Animations', 'Buena Vista', 'CBS Films', 'Crest','DreamWorks SKG',
'DreamWorks Animation','Focus','Happy Madison Productions','Highlight Communications', 
'Legendary Pictures', 'Liberty Starz', 'Lionsgate', 'MGM','Miramax', 'Morgan Creek Productions', 
'New Line Cinema', 'Pixar', 'Regency Enterprises', 'Relativity Media', 'Reliance Entertainment', 
'Sony', 'Spyglass Entertainment', 'Summit Entertainment', 'Vertigo Entertainment','Village Roadshow Pictures',
'Virgin', 'Weinstein Company')='Others'")



summary(movies1[,3])


### Plot Year, Studio and Year vs Studio together

par(mfrow=c(1,3))
## Year plot
barplot(summary(movies1[,1]), ylab="Number of movies", xlab="Year", ylim=c(0,160), main="Barplot of Number of movies in each Year")
yearplot <- barplot(summary(movies1[,1]), ylab="Number of movies", xlab="Year", ylim=c(0,160), main="Barplot of Number of movies in each Year", plot=F)
text(yearplot, summary(movies1[,1])+5, labels=summary(movies1[,1]))
## Studio plot
barplot(summary(movies1[,3]), ylim=c(0,200), las=1, xlab="Type of Studios", ylab="Number of Movies", main="Number of Movies for each type of studios")
studioplot <- barplot(summary(movies1[,3]), ylim=c(0,200), las=1, xlab="Type of Studios", ylab="Number of Movies", main="Number of Movies for each type of studios", plot=F)
text(studioplot, summary(movies1[,3])+5, labels=summary(movies1[,3]))
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

### Plot Story and Genre together
par(mfrow=c(2,1))
par(mar=c(4,10,2,2))
barplot(rev(summary(movies1[,6])), horiz=TRUE, las=1, xlab="Number of Movies", xlim=c(0,70), main="Number of Movies for each Plot Type")
storyplot <- barplot(rev(summary(movies1[,6])), horiz=TRUE, las=1, xlab="Number of Movies", xlim=c(0,70), main="Number of Movies for each Plot Type", plot=F)
text(rev(summary(movies1[,6]))+1.5, storyplot, labels=rev(summary(movies1[,6])))

par(mar=c(4,10,2,2))
genreplot <- barplot(rev(summary(movies1[,7])),xlim=c(0,120), horiz=TRUE, las=1, xlab="Number of Movies", main="Number of Movies for each Genre", plot=F)
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
par(mar=c(5,4,2,2))
plot(movies1[,c(1,14)], ylab="log(WorldwideGross)")
title("log(WorldwideGross) against Year")

## Studios vs. WG
par(mar=c(5,4,2,2))
plot(movies1[,c(3,14)], ylab="log(WorldwideGross)")
title("log(WorldwideGross) against Studio")

## Story vs. WG
par(mar=c(10,4,2,2))
plot(movies1[,c(6,14)], ylab="log(WorldwideGross)", las=3, xlab=NULL)
title("log(WorldwideGross) against Story")

## Genre vs. WG
par(mar=c(10,4,2,2))
plot(movies1[,c(7,14)], ylab="log(WorldwideGross)", las=3, xlab=NULL)
title("log(WorldwideGross) against Genre")

 

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
extractAIC(revenue.lm1)
vif(revenue.lm1)

## Residual Plots
par(mfrow=c(2,2))
plot(density(revenue.lm1$residuals), main="Residuals Density")
plot(revenue.lm1, which=c(1,2,5))

### use best subsets
attach(movies2)
movies.matrix <- cbind(RottenTomatoes, AudienceScore, Budget, CinemasOpenWeek, AverageOpenWeek, OpeningWeekend)
best.revenue <- leaps(movies.matrix, WorldwideGross, nbest =3)
plot(best.revenue$size+1, best.revenue$Cp)
abline(coef=c(0,1))
best.revenue

revenue.lm2 <- step(revenue.lm1)
summary(revenue.lm2)
extractAIC(revenue.lm2)
vif(revenue.lm2)

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

anova(revenue.lm1, revenue.lm2)

## Revenue - with Year and Studio
revenue.lm3 <- lm(WorldwideGross ~ Year + Studio + RottenTomatoes + AudienceScore + Budget + CinemasOpenWeek + AverageOpenWeek + OpeningWeekend, data=movies2)
summary(revenue.lm3)

## Revenue - with Year and Studio and everything
revenue.lm4 <- lm(WorldwideGross ~ Year + Studio + Story + Genre + RottenTomatoes + AudienceScore + Budget + CinemasOpenWeek + AverageOpenWeek + OpeningWeekend, data=movies2)
summary(revenue.lm4)

revenue.lm5 <- step(revenue.lm4)
summary(revenue.lm5)


revenue.lm6 <- lm(WorldwideGross ~ AudienceScore + Budget + AverageOpenWeek + OpeningWeekend, data=movies2)
revenue.lm7 <- lm(WorldwideGross ~ AudienceScore + Budget + AverageOpenWeek + CinemasOpenWeek , data=movies2)
summary(revenue.lm6)
extractAIC(revenue.lm6)
vif(revenue.lm6)
summary(revenue.lm7)
extractAIC(revenue.lm7)
vif(revenue.lm7)
summary(revenue.lm2)



anova(revenue.lm6, revenue.lm2) #### may be this one is a better model
anova(revenue.lm7, revenue.lm2)



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

revenue.lm2 <- step(revenue.lm1)
anova(revenue.lm2, revenue.lm1)


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