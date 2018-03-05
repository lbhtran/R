#### Week 3: Hypothesis Testing ####

#### Transfer cholest.mtv into R ###
chol = read.csv("data/cholest.csv", header = TRUE)

chol
summary(chol) 
names(chol)
attach(chol)

#### Paired t-test for Cholesterol level after 2-days and 4-days ####
t.test(X2.Day,X4.Day, paired=T)

length(X2.Day)

#### Perform hypothesis testing manually ####
change <- X2.Day - X4.Day #difference between 2 samples

change
change_mean=mean(change, na.rm=T)  #mean with remove NA argument
change_sd=sd(change, na.rm=T) #standard deviation

t_obs= (change_mean)/(change_sd/sqrt(28))

t_obs

2*pt(-abs(t_obs), df=27)


#### Independent samples t-test for Cholesterol level after 2-days and control ####

t.test(X2.Day, Control, paired=F)

#### Manually ####

pevar = (27*(sd(X2.Day,na.rm=T))^2 + (length(Control)-1)*(sd(Control))^2)/(28+length(Control)-2)
SE = sqrt(pevar)

t = (mean(X2.Day, na.rm=T)-mean(Control))/(SE*(sqrt(1/28+1/length(Control))))
t

2*pt(-abs(t), df=28+length(Control)-2)
