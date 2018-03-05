##################### FINAL STATISTICS ASSIGNMENT #######################

########## Load all the required packages ############

library("car")
library("leaps")
library("boot")

########## Load data frame ########

movies.origin <- read.csv("data/movies1.csv") ## import dataframe

### create another dataframe for analysis so we can keep the original dataframe for
### reference later on

movies <- movies.origin

movies # display the data

summary(movies)

### Year is not currently a factor as it should be
movies[,1] <- as.factor(movies[,1])	### change Year to factor 

######## check and recalculate WorldwideGross #######
movies[,14] == movies[,12] + movies[,13] ## there is quite a lot of FALSE values for WwG

movies[,14] <- movies[,12] + movies[,13] ## recalculate WwG

movies[,14] == movies[,12] + movies[,13]	## no more error

######## recalculate OpeningWeekend ########
movies.extra <- movies
movies[,11] <- (movies[,9] * movies[,10]) /1000000

######## Treating the missing values ########
## iterate all the rows with missing values
movies.na <- movies[!complete.cases(movies),]
dim(movies.na) ## there are 37 cases with missing values
movies.na ### list of all incomplete observation

sapply(movies, function(x) sum(is.na(x))) ## count number of NA in each variable

### store the number of NA in a new variable for analysis
missing <- apply(movies, 1, function(x) sum(is.na(x))) 
missing <- as.factor(missing) ### make the variable categorical
summary(missing) ### summary details of the new variables
movies.a <- cbind(movies, missing) ## add to a new temporary dataframe to analyse the NA values

names(movies.a)

## Get the distribution with log(WwG)
boxplot(log10(movies.a[,14])~movies.a[,17], ylab="log(WorldwideGross)", xlab="Number of missing values", main="Distribution of WorldwideGross \nby number of missing values")

#### The number of values with 3 NA's has lower log(WorldwideGross) than others

movies.a[missing=='3',] ### Check what they are
### essentially, they are those values with both CinemasOpenWeek and AverageOpenWeek
### missing. Hence, when we perform recalculation, OpeningWeekend becomse NA as well
### Let's find these values and plot their original values of OpeningWeekend against
### WorldwideGross to see if their original values have any significance.

### find them in the orginical dataframe to see what happens

movies.OW <- movies.origin[c('7','12','13','14','24','35','65','96','138'),]

summary(movies.OW[,14])

plot(log10(movies.OW[,14]) ~ log10(movies.OW[,11])) 
### there is not much of a visible correlation in this plot, it may be slightly positive
### correlation
cor(log10(movies.OW[,14]), log10(movies.OW[,11])) 
### cor = 0.5, however we only have 9 observations

boxplot(log10(movies.OW[,14]), log10(movies[,14]))
### as shown in the previous plot, WorldwideGross in these 9 values is lower than 
### WorldwideGross values of the whole dataframe.
### However, if we don't omit these values, there is no way to obtain values for
### CinemasOpenWeek and AverageOpenWeek. Hence, it is more sensible to omit these
### missing observations rather than omit both variables out of the analysis.

### try to plot WorldwideGross against OpeningWeekend with full data and color coded the missing values
missing3 <- ifelse(movies.a[,17]==3, 1, 0)
movies.a1 <- cbind(movies.extra,missing3)
par(mar=c(4,4,2,2))
plot(log10(movies.a1[,11]), log10(movies.a1[,14]), col=c("blue","red")[1+movies.a1[,17]], ylab="log(WorldwideGross)", xlab="log(OpeningWeekend)")
plot(log10(movies.a1[,8]), log10(movies.a1[,14]), col=c("blue","red")[1+movies.a1[,17]], ylab="log(WorldwideGross)", xlab="log(Budget)")
plot(movies.a1[,4], log10(movies.a1[,14]), col=c("blue","red")[1+movies.a1[,17]], ylab="log(WorldwideGross)", xlab="RottenTomatoes")
plot(movies.a1[,5], log10(movies.a1[,14]), col=c("blue","red")[1+movies.a1[,17]], ylab="log(WorldwideGross)", xlab="AudienceScore")

### load the new file with missing values edited
movies.origin2 <- read.csv("data/movies1a.csv") ## import dataframe

### create another dataframe for analysis so we can keep the original dataframe for
### reference later on

movies.0 <- movies.origin2

movies.0 # display the data

summary(movies.0)

### Year is not currently a factor as it should be
movies.0[,1] <- as.factor(movies.0[,1])	### change Year to factor 

movies.0[,14] <- movies.0[,12] + movies.0[,13] ## recalculate WwG

######## recalculate OpeningWeekend ########
movies.0[,11] <- (movies.0[,9] * movies.0[,10]) /1000000

sapply(movies.0, function(x) sum(is.na(x))) ## count number of NA in each variable

### store the number of NA in a new variable for analysis
missing <- apply(movies.0, 1, function(x) sum(is.na(x))) 
missing <- as.factor(missing) ### make the variable categorical
summary(missing) ### summary details of the new variables
movies.a2 <- cbind(movies.0, missing) ## add to a new temporary dataframe to analyse the NA values

### Boxplot against numerical predictors
par(mfrow=c(2,3))
boxplot(movies.a[,4]~movies.a[,17], ylab="RottenTomatoes", xlab="Number of missing values")
boxplot(movies.a[,5]~movies.a[,17], ylab="AudienceScores", xlab="Number of missing values")
boxplot(log10(movies.a[,8])~movies.a[,17], ylab="log(Budget)", xlab="Number of missing values")
boxplot(movies.a[,9]~movies.a[,17], ylab="CinemasOpenWeek", xlab="Number of missing values")
boxplot(log10(movies.a[,10])~movies.a[,17], ylab="log(AverageOpenWeek)", xlab="Number of missing values")
boxplot(log10(movies.a[,11])~movies.a[,17], ylab="log(OpeningWeekend)", xlab="Number of missing values")

### Check the distribution of missing data with other predictors
### Against Categorical
Yearvsmissing <- with(movies.a, table(Year, missing))
Yearvsmissing
Studiovsmissing <- with(movies.a, table(Studio, missing))
Studiovsmissing ### if remove, Reliance Entertainment no longer presents in the dataset
Storyvsmissing <- with(movies.a, table(Story, missing))
Storyvsmissing
Genrevsmissing <- with(movies.a, table(Genre, missing))
Genrevsmissing


## remove all the cases with missing values
movies1 <- na.omit(movies.0)
movies.b <- na.omit(movies.0) ### have another dataframe for back-up
dim(movies1) ## there are 383 movies left

summary(movies1)

### Numerical Variables
summary(movies1[,c(4,5,8,9,10,11,12,13,14)])
sapply(movies1[,c(4,5,8,9,10,11,12,13,14)],sd) ## Standard deviation

### Plots for Discrete
par(mfrow=c(3,1),mar=c(4,4,1,2))
hist(movies1[,4], xlim=c(0,100), ylim=c(0,100), xlab="RottenTomatoes", main=NULL)
hist(movies1[,5], xlim=c(0,100), ylim=c(0,100), xlab="AudienceScores", main=NULL)
hist(movies1[,9], xlim=c(0,5000), ylim=c(0,120), xlab="CinemasOpenWeek", main=NULL)

### Plots for continuous
par(mfrow=c(2,3), mar=c(4,4,1,2))
boxplot(movies1[,8], xlab="Budget", ylab="Million USD", main=NULL)
boxplot(movies1[,10], xlab="AverageOpenWeek", ylab="USD", main=NULL)
boxplot(movies1[,11], xlab="OpeningWeekend", ylab="Million USD", main=NULL)
boxplot(movies1[,12], xlab="DomesticGross", ylab="Million USD", main=NULL)
boxplot(movies1[,13], xlab="ForeignGross", ylab="Million USD", main=NULL)
boxplot(movies1[,14], xlab="WorldwideGross", ylab="Million USD", main=NULL)

### transformation
par(mfrow=c(2,2), mar=c(4,4,1,2))
boxplot(log10(movies1[,8]), xlab="log(Budget)", main=NULL)
boxplot(log10(movies1[,10]), xlab="log(AverageOpenWeek)", main=NULL)
boxplot(log10(movies1[,11]), xlab="log(OpeningWeekend)", main=NULL)
boxplot(log10(movies1[,14]), xlab="log(WorldwideGross)", main=NULL)

### Categorical Variables

### Year
summary(movies1[,1])

### Studio
summary(movies1[,3])

#Too many category in Studio, one idea is to use use 'car' to collapse some into Major, Independent and Others
require("car")    
### the Major category contains those studios that are considered to be one of the 6
### major studios in Hollywood (Source: Wikipedia)
movies1[,3] <- recode(movies1[,3], "c('Warner Bros.','Disney','Universal','Columbia','20th Century Fox','Paramount')='Major'")

summary(movies1[,3])

### for the rest of the studios, some of them are mini-major studios in Hollywood,
### some are a joint-venture of 2 majors. Hence, for easier for categorising, let's have
### a category called Others for the rest of these studios
movies1[,3] <- recode(movies1[,3], "c('Aardman Animations', 'Buena Vista', 'CBS Films', 'Crest','DreamWorks','DreamWorks Animation','Focus','Happy Madison Productions','Highlight Communications', 'Legendary Pictures', 'Liberty Starz', 'Lionsgate', 'MGM','Miramax', 'Morgan Creek Productions', 'New Line Cinema', 'Pixar', 'Regency Enterprises', 'Relativity Media', 'Reliance Entertainment', 'Sony', 'Spyglass Entertainment', 'Summit Entertainment', 'Vertigo Entertainment','Village Roadshow Pictures','Virgin', 'Weinstein Company')='Others'")

summary(movies1[,3])

### Story
summary(movies1[,6])

### Genre
summary(movies1[,7])

### Plot for Categorical

layout(matrix(c(1,2,1,2,3,3,3,3,3,3,4,4,4,4), 7,2,byrow=T))
par(mar=c(4,4,1,2), cex.axis=1)
yearplot <- plot(movies1[,1], ylab="Number of movies", xlab="Year", ylim=c(0,160), main=NULL)
text(yearplot, summary(movies1[,1])+7, labels= summary(movies1[,1]))

par(mar=c(4,4,1,2), cex.axis=0.7)
studioplot <- plot(movies1[,3],  ylab="Number of movies", xlab="Studio Type", ylim=c(0,200), main=NULL, las=1)
text(studioplot, summary(movies1[,3])+7, labels= summary(movies1[,3]))

par(mar=c(4,6,2,2),cex.axis = 0.65)
storyplot <- barplot(rev(summary(movies1[,6])), horiz=TRUE, las=1, xlab="Number of Movies", xlim=c(0,70), main="Number of Movies for each Plot Type")
text(rev(summary(movies1[,6]))+3, storyplot, labels=rev(summary(movies1[,6])))

par(mar=c(4,6,2,2),cex.axis = 0.7)
genreplot <- barplot(rev(summary(movies1[,7])),xlim=c(0,110), horiz=TRUE, las=1, xlab="Number of Movies", main="Number of Movies for each Genre")
text(rev(summary(movies1[,7]))+3, genreplot, labels=rev(summary(movies1[,7])))

### Bivariate Relationship

## between numerical variable

pairs(movies1[,c(4,5,8,9,10,11,14)], cex=0.5)
### transform the cont. variables
movies.matrix <- cbind(movies1[,c(4,5,9)],log10(movies1[,c(8,10,11,14)]))
movies.matrix <- movies.matrix[c(1,2,4,3,5,6,7)]
pairs(movies.matrix, cex=0.5)

### correlation coefficients after variables transformation
cor(movies.matrix)

### Plots against Worldwide Gross
par(mfrow=c(2,2))

## Year vs. WG
par(mar=c(4,4,1,2))
plot(log10(movies1[,14])~movies1[,1], ylab="log(WorldwideGross)", xlab='Year')

## Studios vs. WG
par(mar=c(4,4,1,2))
plot(log10(movies1[,14])~movies1[,3], ylab="log(WorldwideGross)", xlab='Studio Type')

## Story vs. WG
par(mar=c(6,4,1,2), cex.axis = 0.65)
plot(log10(movies1[,14])~movies1[,6], ylab="log(WorldwideGross)", las=3, xlab=NULL)

## Genre vs. WG
par(mar=c(6,4,1,2), cex.axis = 0.7)
plot(log10(movies1[,14])~movies1[,7], ylab="log(WorldwideGross)", las=3, xlab=NULL)




####### LINEAR REGRESSION MODEL ########

### Revenue - No Categorical Variables
revenue.lm1 <- lm(log10(WorldwideGross) ~ RottenTomatoes + AudienceScore + log10(Budget) + CinemasOpenWeek + log10(AverageOpenWeek) + log10(OpeningWeekend), data=movies1)
summary(revenue.lm1)

#### These plots do not show anything
plot(movies.b[,3], rstandard(revenue.lm1), col=1:lenght(movies.b[,3])

plot(log10(WorldwideGross) ~ AudienceScore, pch=16, col=Studio, data=movies.b)
legend('topleft', paste(movies.b$Studio), col=1:length(movies.b$Studio),pch=16)

plot(log10(WorldwideGross) ~ log10(Budget), pch=16, col=Studio, data=movies.b)
### end


movies.subset <- regsubsets(log10(WorldwideGross) ~ RottenTomatoes + AudienceScore + log10(Budget) + CinemasOpenWeek + log10(AverageOpenWeek) + log10(OpeningWeekend), data=movies1)
summary(movies.subset)
subsets(movies.subset, statistic='cp', legend=T, cex=0.5)
abline(0,1)

#### Full model

revenue.full <- lm(log10(WorldwideGross) ~ Year + Studio + Story + Genre + RottenTomatoes + AudienceScore + log10(Budget) + CinemasOpenWeek + log10(AverageOpenWeek) + log10(OpeningWeekend), data=movies1)
summary(revenue.full)

movies.subset2 <- regsubsets(log10(WorldwideGross) ~ Year + Studio + Story + Genre + RottenTomatoes + AudienceScore + log10(Budget) + CinemasOpenWeek + log10(AverageOpenWeek) + log10(OpeningWeekend), data=movies1)
summary(movies.subset2)

subset.summary <- summary(movies.subset2)

subsets(movies.subset2, statistic='cp', legend=F, cex=0.5, xlim=c(0,9))
abline(0,1)

### new lm from bestsubsets
revenue.lm2 <- lm(log10(WorldwideGross) ~ AudienceScore + log10(Budget) + CinemasOpenWeek + log10(AverageOpenWeek) + log10(OpeningWeekend), data=movies1)
summary(revenue.lm2)

## Residual Plots
par(mfrow=c(2,2))
plot(density(revenue.lm2$residuals))
plot(revenue.lm2, which=c(1,2,5))

movies1['285',]

movies2 <- movies1[which(movies1$Film != "Paranormal Activity"),]

### run lm without the observation
revenue.lm3 <- lm(log10(WorldwideGross) ~ AudienceScore + log10(Budget) + CinemasOpenWeek + log10(AverageOpenWeek) + log10(OpeningWeekend), data=movies2)
summary(revenue.lm3)

## Residual Plots
par(mfrow=c(2,2))
plot(density(revenue.lm3$residuals), main="Residual Density")
plot(revenue.lm3, which=c(1,2,5))

movies2[c('373','358','337'),]
### The King's Speech has low budget but high Gross?
con.king <- predict(revenue.lm3, movies2['373',], interval="confidence")
pred.king <- predict(revenue.lm3, movies2['373',], interval="predict")
10^con.king
10^pred.king

### Gulliver's Travels has low AudienceScore but high Gross?
con.travel <- predict(revenue.lm3, movies2['358',], interval="confidence")
pred.travel <- predict(revenue.lm3, movies2['358',], interval="predict")
10^con.travel
10^pred.travel

### Johnny English Reborn has low budget but high Gross?
con.reborn <- predict(revenue.lm3, movies2['337',], interval="confidence")
pred.reborn <- predict(revenue.lm3, movies2['337',], interval="predict")
10^con.reborn
10^pred.reborn

### Check for a random value in the dataframe: A Perfect Getaway
con.perfect <- predict(revenue.lm3, movies2['66',], interval="confidence")
pred.perfect <- predict(revenue.lm3, movies2['66',], interval="predict")
10^con.perfect
10^pred.perfect

### the lm for the profit

profit.lm <- lm(log10(WorldwideGross) - log10(Budget) ~ AudienceScore + log10(Budget) + CinemasOpenWeek + log10(AverageOpenWeek) + log10(OpeningWeekend), data=movies2)
summary(profit.lm)
### all the coefficients are the same except for log10(Budget), all variables are significant
## Residual Plots
par(mfrow=c(2,2))
plot(density(profit.lm$residuals))
plot(profit.lm, which=c(1,2,5))


### Logistic Regression
## create a new binary factor

nonUS <- ifelse(movies1$DomesticGross <= movies1$ForeignGross, 1, 0)
movies3 <- cbind(movies1,nonUS)
movies3

movies3$nonUS <- as.factor(movies3$nonUS)

plot(log10(movies3[,8]) ~ movies3$nonUS)

attach(movies3)

plot(log10(movies3[,8])~log10(OpeningWeekend), col=c("blue","red")[1+nonUS])

detach(movies3)

revenue.glm1 <- glm(nonUS ~ RottenTomatoes + AudienceScore + log10(Budget) + CinemasOpenWeek + log10(AverageOpenWeek) + log10(OpeningWeekend), family = "binomial", data=movies3)

summary(revenue.glm1)

revenue.glm2 <- glm(nonUS ~ RottenTomatoes + AudienceScore + log10(Budget) + CinemasOpenWeek + log10(OpeningWeekend), family = "binomial", data=movies3)

summary(revenue.glm2)

revenue.glm3 <- glm(nonUS ~ AudienceScore + log10(Budget) + CinemasOpenWeek + log10(OpeningWeekend), family = "binomial", data=movies3)

summary(revenue.glm3)

revenue.glm4 <- glm(nonUS ~ AudienceScore + log10(Budget) + log10(OpeningWeekend), family = "binomial", data=movies3)

summary(revenue.glm4)

revenue.glm5 <- glm(nonUS ~ log10(Budget) + log10(OpeningWeekend), family = "binomial", data=movies3)

summary(revenue.glm5)

revenue.glmstep1 <- step(revenue.glm1)

summary(revenue.glmstep1)

## Residual Plots
par(mfrow=c(2,2))
plot(density(revenue.glm5$residuals))
plot(revenue.glm5, which=c(1,2,5))


movies4 <- movies3[which(movies3$Film != "Paranormal Activity"),]

attach(movies4)
plot(log10(movies4[,8])~log10(movies4[,12]), col=c("blue","red")[1+nonUS])
detach(movies4)

revenue.glm6 <- glm(nonUS ~ log10(Budget) + log10(OpeningWeekend), family = "binomial", data=movies4)

summary(revenue.glm6) 

par(mfrow=c(2,2))
plot(density(revenue.glm5$residuals))
plot(revenue.glm5, which=c(1,2,5), col=c("blue","red")[1+movies3[,17]])


revenue5.diag <- glm.diag(revenue.glm5)
glm.diag.plots(revenue.glm5,revenue5.diag)


par(mfrow=c(2,2))
plot(density(revenue.glm6$residuals))
plot(revenue.glm6, which=c(1,2,5), col=c("blue","red")[1+movies4[,17]])

### Check if there is any possibility of higher order
plot(revenue.glm6, which=1, col=c("blue","red")[1+movies4[,17]])

### plot residuals against each explanatory variable
X1 <- log10(movies4[,8])
X2 <- log10(movies4[,11])
Y <- movies4[,17]

plot(X1,residuals(revenue.glm6), col=c("blue","red")[1+Y])
lines(lowess(X1,residuals(revenue.glm6)), col="black", lwd=2)
lines(lowess(X1[Y==0], residuals(revenue.glm6)[Y==0]), col="blue")
lines(lowess(X1[Y==1], residuals(revenue.glm6)[Y==1]), col="red")
### not much different from the first plot

plot(X2,residuals(revenue.glm6), col=c("blue","red")[1+Y])
lines(lowess(X2,residuals(revenue.glm6)), col="black", lwd=2)
lines(lowess(X2[Y==0], residuals(revenue.glm6)[Y==0]), col="blue")
lines(lowess(X2[Y==1], residuals(revenue.glm6)[Y==1]), col="red")
### much more interesting, may suggest that there is something going on with this variable and we could try to add a quadratic term into the model

revenue.glm7 <- glm(nonUS ~ log10(Budget) + log10(OpeningWeekend) + (log10(OpeningWeekend))^2, family = "binomial", data=movies4)

summary(revenue.glm7)

plot(revenue.glm7, which=1, col=c("blue","red")[1+movies4[,17]])

glm.all <- cbind(revenue.glm5, revenue.glm6, revenue.glm7)

extractAIC(revenue.glm3)
extractAIC(revenue.glm4)
extractAIC(revenue.glm5)
extractAIC(revenue.glm6)
extractAIC(revenue.glm7)

sapply(glm.all,extractAIC)

#### Extra

### glm3

glm3 <- glm(nonUS ~ AudienceScore + log10(Budget) + CinemasOpenWeek + log10(OpeningWeekend), family = "binomial", data=movies4)

summary(glm3)

A1 <- movies4[,4]
A2 <- log10(movies4[,8])
A3 <- movies4[,9]
A4 <- log10(movies4[,11])
B <- movies4[,17]

plot(glm3, which=1, col=c("blue","red")[1+movies4[,17]])
plot(A1,residuals(glm3), col=c("blue","red")[1+B])
lines(lowess(A1,residuals(glm3)), col="black", lwd=2)
lines(lowess(A1[B==0], residuals(glm3)[B==0]), col="blue")
lines(lowess(A1[B==1], residuals(glm3)[B==1]), col="red")

plot(A2,residuals(glm3), col=c("blue","red")[1+B])
lines(lowess(A2,residuals(glm3)), col="black", lwd=2)
lines(lowess(A2[B==0], residuals(glm3)[B==0]), col="blue")
lines(lowess(A2[B==1], residuals(glm3)[B==1]), col="red")

plot(A3,residuals(glm3), col=c("blue","red")[1+B])
lines(lowess(A3,residuals(glm3)), col="black", lwd=2)
lines(lowess(A3[B==0], residuals(glm3)[B==0]), col="blue")
lines(lowess(A3[B==1], residuals(glm3)[B==1]), col="red")

plot(A4,residuals(glm3), col=c("blue","red")[1+B])
lines(lowess(A4,residuals(glm3)), col="black", lwd=2)
lines(lowess(A4[B==0], residuals(glm3)[B==0]), col="blue")
lines(lowess(A4[B==1], residuals(glm3)[B==1]), col="red")

### trial 1

glm1 <- glm(nonUS ~ Year + AudienceScore + log10(Budget) + CinemasOpenWeek + log10(OpeningWeekend), family = "binomial", data=movies4)
summary(glm1)

A0 <- movies4[,1]
plot(A0,residuals(glm1))

plot(A0,residuals(glm1), col=c("blue","red")[1+B])
lines(lowess(A0,residuals(glm1)), col="black", lwd=2)
lines(lowess(A0[B==0], residuals(glm1)[B==0]), col="blue")
lines(lowess(A0[B==1], residuals(glm1)[B==1]), col="red")

glm0 <- glm(nonUS ~ Year + RottenTomatoes + AudienceScore + log10(Budget) + CinemasOpenWeek + log10(AverageOpenWeek) + log10(OpeningWeekend), family = "binomial", data=movies4)
summary(glm0)

glmA <- step(glm0)
summary(glmA)

plot(glmA, which=1, col=c("blue","red", "green")[movies4[,1]])

### LM extra

### Revenue - No Categorical Variables
extra.lm <- lm(log10(WorldwideGross) ~ RottenTomatoes + AudienceScore + log10(Budget) + CinemasOpenWeek + log10(AverageOpenWeek) + log10(OpeningWeekend), data=movies.a1)
summary(extra.lm)

extra.lms <- step(extra.lm)
summary(extra.lms)


movies.original[which(movies.original$Film != "Paranormal Activity"),]
movies.origin[movies.origin$Film == "A Dangerous Method",]