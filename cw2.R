#### Question 2

## a. Fitting rmodel 1 with treatment as fixed effect and blend as random effect with MLE

library(faraway)
data(penicillin)
penicillin[1:10,]

rmodel1 = lmer(yield ~ treat + (1|blend), data=penicillin, REML=F)
summary(rmodel1)
anova(rmodel1)

# fit a reduced rmodel2 with only blend as random effect with MLE

rmodel2 = lmer(yield ~ (1|blend), data=penicillin, REML=F)
summary(rmodel2)

# likelihood ratio test

-2*(logLik(rmodel2)-logLik(rmodel1))
1-pchisq(4.047367,3) ## get p-value
### conclusion: significant with 25% level

## b. Fitting model3 with only treatment as fixed effect
model3 = lm(yield ~ treat, data=penicillin)
summary(model3)

-2*(logLik(model3)-logLik(rmodel1))
1-pchisq(3.453634,5) ## signicant

## Use parametric bootstrapping to test H0: sigma^2_blend=0
lrt <- c()	## create empty vector
rmodel=lmer(yield~treat+(1|blend),data=penicillin)	## rand. effects model
lmodel=lm(yield~treat,data=penicillin)			## model under H0
LR_Value <- 2*(logLik(rmodel)-logLik(lmodel,REML=T)) ## The likelihood ratio value
LR_Value
simulate(lmodel) ## illustrate function to simulate response values from a fitted model (in this case the model under H0)
unlist(simulate(lmodel))
## Do the bootstrapping
for(i in 1:1000){
	y_tilde <- unlist(simulate(lmodel))
	lmod <- lm(y_tilde~treat,data=penicillin)
	rmod <- lmer(y_tilde~treat+(1|blend),data=penicillin)
	lrt[i] <- 2*(logLik(rmod)-logLik(lmod,REML=TRUE))
}
plot(density(lrt)) ## smoothed histogram of the distribution of LR under H0
abline(v=LR_Value) ## Observed value of LR
mean(lrt > LR_Value) ## estimated p-value


#### Question 3

glmodel <- glm(test ~ IQ + ses + Class, family=gaussian, data=pupils)
summary(glmodel)

anova(glmodel, test="F")


lmodel = lm(test~IQ+ses+Class, data=pupils)
summary(lmodel)

rmodel1 <- lmer(test~IQ+ses+(1|Class), data=pupils)
summary(rmodel1)

x = ranef(rmodel1)
plot(density(x$Class[,1]))

qqnorm(x$Class[,1])
qqline(x$Class[,1])

yhat = fitted(rmodel1)
rawresid = resid(rmodel1)

qqnorm(rawresid)
qqline(rawresid)

plot(yhat, rawresid)

library(lme4)

### Fit a model with random slope for variable IQ
pupils[1:10,]
rmodel4 = lmer(test~ses+IQ+(1+IQ|Class),data=pupils )
summary(rmodel4)
lmodel=lm(test~ses+IQ+Class,data=pupils ) ## fit model without random effects to compare the residual variances
summary(lmodel)
summary(model)
ranef(model) ## function to get the random effects
plot(1:131,ranef(model)$Class[,2]) ## plot the random effects for IQ to identify (potentially) "bad" classes, i.e. ones that hinder clever students.

rmodel5 = lmer(test~ses+IQ+(1+IQ|Class),data=pupils, REML=F )
summary(rmodel5)


-2*(logLik(rmodel4)-logLik(rmodel1))



##Question 4

library(lme4)

factor(hip$ses)

hip[0:10,]
names(hip)

glmodel = glm(Nfract~offset(log(Npop))+ses+municipality+sex, data=hip, family=poisson)
summary(glmodel)

glmodel1= glmer(Nfract~offset(log(Npop)) + ses + sex + (1|municipality), data=hip, family=poisson)
summary(glmodel1)

res = resid(glmodel1)
rand = ranef(glmodel1)

qqnorm(resid)
qqline(res)

qqnorm(rand$municipality[,1])
qqline(rand$municipality[,1])

-2*(logLik(glmodel1)-logLik(glmodel))
1-pchisq(113188.9,3)

1-pt(10.824,2281)
1-pt(26.122,2281)
1-pt(3.411,2281)
1-pt(-1.372,2281)
1-pt(-2.729,2281)


nbglmodel <- glm.nb(y~1)
summary(nbglmodel)

### Question 3
library(lme4)

rmodel <- lmer(test~ses+IQ+(1|Class),data=pupils, REML=F)
rmodel2 <- lmer(test~ses+IQ+(1+IQ|Class),data=pupils, REML=F)
LR_Value <- 2*(logLik(rmodel2)-logLik(rmodel))
LR_Value
1-pchisq(15.93184,7)

 
lrt <- c()
## Do the bootstrapping
for(i in 1:1000){
	y_tilde <- unlist(simulate(rmodel))
	rmod1 <- lmer(test~ses+IQ+(1|Class),data=pupils)
	rmod2 <- lmer(test~ses+IQ+(1+IQ|Class),data=pupils)
	lrt[i] <- 2*(logLik(rmod2)-logLik(rmod1))
}
plot(density(lrt)) ## smoothed histogram of the distribution of LR under H0
abline(v=LR_Value) ## Observed value of LR
mean(lrt > LR_Value) ## estimated p-value



rmodel=lmer(yield~treat+(1|blend),data=penicillin)	## rand. effects model
lmodel=lm(yield~treat,data=penicillin)			## model under H0
LR_Value <- 2*(logLik(rmodel)-logLik(lmodel,REML=T)) ## The likelihood ratio value



lmodel <- lm(test ~ ses + IQ + IQ.class + ClassSize + comb, data=pupils)
summary(lmodel)



glmodel1= glmer(Nfract~offset(log(Npop)) + ses + sex + (1|municipality), data=hip, family=poisson)
glmodel = glm(Nfract~offset(log(Npop))+ses +municipality+sex, data=hip, family=poisson)
2*(logLik(glmodel1)-logLik(glmodel))
1-pchisq(1208.24,5)

nb<-negative.binomial(0.6404,link="log")
lambda <- rgamma(200,3,0.5)
y <- rpois(200, lambda)

nbmodel <- glm.nb(y~1)
summary(nbmodel)
