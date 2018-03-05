### Mock project 

cyprus <- read.csv("data/CyprusDisclosure.csv") ## import dataframe
cyprus # display the data

## the first column C1 is the indices, hence we can get rid of it using
cyprus$C1 <- NULL

names(cyprus) ## this line is just to shorten the code when modelling

### Make a matrix plot of everything agains everything
### this is good to see if there is any corellation or 
### influence of the data

pairs(cyprus)

### (1) strong correllation between index and actual, index and max
### actual and max. This is predicted as index is the ratio of actual
### over max.
### (2) something is happenning between sales and assets however we
### may need to transform the data in order to make it clear
### (3) roce seems to affect actual, max and index as well
### (4) not very clear to see if the categorical factors have any
### effect on other variables

## Make a boxplot of all data
boxplot(cyprus)
### the variable for profit margin has a weird long left tail so 
### I make a histogram to see what is happenning. 
hist(cyprus$prmarg)
### Also a summary and its variance
summary(cyprus$prmarg)
var(cyprus$prmarg)
### Sales and assets have long right tail, positively skewed
### we can try to fix this by taking the log of these variables
logsales <- log(cyprus$sales)
plot(cyprus$index, logsales)
boxplot(logsales)
### the plots seem to be more spreaded out
### We may as well do the same for assets
logassets <- log(cyprus$assets)

### as the profit margin is skewed to the left, we may want to 
### transform this data, however, the log function only works with
### positive data. I will leave it as it is for now


## First model: let's plot everything against everything and see if we
## can drop anything

cyprusmodel1 <- lm(index~logsales+logassets+age+prmarg+roce+current+sector+listing+audit, data=cyprus)
### View the summary
summary(cyprusmodel1)
anova(cyprusmodel1)
extractAIC(cyprusmodel1)

par(mfrow=c(2,2))
plot(density(cyprusmodel1$residuals))
plot(newcyprus2.lm, which=c(1,2,5))


cyprus.lm2 <- lm(index~logsales+logassets+prmarg+roce+current+sector+listing+audit, data=cyprus)
extractAIC(cyprus.lm2)

cyprus.lm3 <- lm(index~ age+roce+sector+listing+audit, data=cyprus)
extractAIC(cyprus.lm3)

cyprus.lm4 <- lm(index~ sector+listing+audit, data=cyprus)
extractAIC(cyprus.lm4)

# get package cars for calculating VIF
install.packages("car")
vif(cyprusmodel1)

# another way to calculate VIF
lma <- lm(current~ logsales+logassets+age+prmarg+roce+sector+listing+audit, data=cyprus)
ls <- summary(lma)$r.squared
VIFlogsales <- 1/(1-ls)
VIFlogsales

### not possible to do stepwise as there are missing value. Or we can take age, which contains missing values and try to run a stepwise regression to see
step(lm(index~logsales+logassets+prmarg+roce+current+sector+listing+audit, data=cyprus, na.action=na.omit))

step(cyprusmodel1)

### the model this smallest AIC value in the end is concluded to be index~logsales+roce+sector+listing+audit

cyprusmodel2 <- lm(index~roce+sector+listing+audit, data=cyprus)
summary(cyprusmodel2)
AIC(cyprusmodel2)
vif(cyprusmodel2)

### diagnostic plots
par(mfrow=c(2,2))
plot(density(cyprusmodel2$residuals))
plot(cyprusmodel2, which=c(1,2,5))



### try a logistic regression

library(MASS)

cyprusglm = glm(cbind(actual, max) ~ roce+sector+listing+audit,family=binomial(logit), data=cyprus)

summary(cyprusglm)

### omit data

newcyprus <- na.omit(cyprus)

newcyprus

logsales2 <- log(newcyprus$sales)
logassets2 <- log(newcyprus$assets)

step(lm(index~logsales2+logassets2+prmarg+age+roce+current+sector+listing+audit, data=newcyprus))

newcyprus.lm <- lm(index~logassets+sector+listing+audit, data=cyprus)
newcyprus.lm2 <- lm(index~logassets2+sector+listing+audit, data=newcyprus)

summary(newcyprus.lm)
extractAIC(newcyprus.lm)
extractAIC(cyprusmodel2)
extractAIC(newcyprus.lm2)

summary(cyprusmodel2)
summary(newcyprus.lm2)

par(mfrow=c(2,2))
plot(density(newcyprus.lm$residuals))
plot(newcyprus.lm, which=c(1,2,5))


newcyprus2.lm <- lm(index~sector+listing+audit, data=newcyprus)

summary(newcyprus2.lm)
extractAIC(newcyprus2.lm)
AIC(newcyprus.lm, newcyprus2.lm)

par(mfrow=c(2,2))
plot(density(newcyprus2.lm$residuals))
plot(newcyprus2.lm, which=c(1,2,5))

is.numeric(newcyprus)
as.numeric(newcyprus)
ncyprus <- sapply(newcyprus,as.numeric) #make numeric for correlation
is.numeric(ncyprus)
### find the correlation in R
ncyprus <- subset(newcyprus,select=-c(actual,max))
ncyprus <- sapply(ncyprus,as.numeric)
cor(ncyprus)


ncyprus <- subset(cyprus,select=-c(actual,max))
ncyprus <- sapply(ncyprus,as.numeric)
cor(ncyprus)
pairs(ncyprus)

newcyprus <- na.omit(ncyprus)

logsales2 <- log(newcyprus$sales)
logassets2 <- log(newcyprus$assets)




### Best subsets search by leaps

cyprus.matrix <- cbind(logsales,logassets,cyprus$prmarg,cyprus$roce,cyprus$current,cyprus$sector,cyprus$listing,cyprus$audit)
cyprus.subsets <- leaps(cyprus.matrix,cyprus$index,nbest=3)
plot(cyprus.subsets$size+1, cyprus.subsets$Cp)
abline(coef=c(0,1))



