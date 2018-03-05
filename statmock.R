### Mock project 

cyprus <- read.csv("data/CyprusDisclosure.csv") ## import dataframe
cyprus # display the data

## the first column C1 is the indices, hence we can get rid of it using
cyprus$C1 <- NULL

names(cyprus) ## this line is just to shorten the code when modelling

### Make a matrix plot of everything agains everything
### this is good to see if there is any corellation or 
### influence of the data

cyprus <- subset(cyprus,select=-c(actual,max)) ## take a subset without actual and max
cyprus <- sapply(cyprus,as.numeric) ## make the subset a numeric matrix


pairs(cyprus)

### (1) strong correllation between index and actual, index and max
### actual and max. This is predicted as index is the ratio of actual
### over max.
### (2) something is happenning between sales and assets however we
### may need to transform the data in order to make it clear
### (3) roce seems to affect actual, max and index as well
### (4) not very clear to see if the categorical factors have any
### effect on other variables

## Make a boxplot of all data except sector, lising and audit as they are factors
boxplot(cyprus)
boxplot(subset(cyprus,select=-c(sector,listing,audit)))

boxplot(subset(cyprus,select=-c(actual,max,age,sector,listing,audit)))


### draw boxplot specifically for sales and assets
boxplot(subset(cyprus,select=c(sales, assets,current)))
### or a histogram
hist(cyprus$sales)

### transform sales, assets and current
logsales <- log(cyprus$sales)
logassets <- log(cyprus$assets)
logcurrent <- log(cyprus$current)

### look at their boxplot again

boxplot(logsales,logassets,logcurrent, names=c("logsales","logassets","logcurrent"))

par(mfrow=c(1,2))
boxplot(subset(cyprus,select=c(sales, assets,current)))
boxplot(logsales,logassets,logcurrent, names=c("logsales","logassets","logcurrent"))

### add the new variables into the data frame
cyprus <- cbind(cyprus,logsales, logassets, logcurrent)
cyprus

### look at the matrix plot again
pairs(subset(cyprus,select=-c(actual, max, sales, assets, current,sector,listing,audit)))


summary(cyprus$prmarg)



1/cyprus$prmarg
logprmarg <- log(100-cyprus$prmarg)
boxplot(logprmarg, log="y")


cyprus <- cbind(cyprus, logprmarg)


### as profit margin is the ratio of profit to sales, we could tranform this and find the profit instead
profit <- cyprus$prmarg*cyprus$sales

boxplot(profit)
summary(profit)

logprofit <- log(profit + 60)
boxplot(logprofit)

cyprus.a <- cbind(cyprus, logprofit)

pairs(subset(cyprus.a,select=-c(actual, max, sales, assets, current,sector,listing,audit,logprmarg,prmarg)))


cyprus.lm <- lm(index ~, data=cyprus.a)
summary(cyprus.lm)

### roce is profit over capital

capital <- profit/cyprus$roce
boxplot(capital)

logcapital <- log(capital)

is.na(capital)



