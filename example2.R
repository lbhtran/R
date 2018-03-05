library(lme4)
### Test for the overall significance of the fixed effect "tyres" in a model with car_type as a random effect
rmodel=lmer(wear~tyre+(1|car_type),data=tyres) # model with car_type as random effect and tyres as fixed effect
summary(rmodel)
rmodel=lmer(wear~tyre+(1|car_type),data=tyres,REML=F) ## Fit it again with max. likelihood
summary(rmodel)
rmodel2=lmer(wear~(1|car_type),data=tyres,REML=F) ## Fit it again without tyres
-2*(logLik(rmodel2)-logLik(rmodel)) ## Likelihood ratio test
1-pchisq(30.93647,3) ## get p-value

### Use likel. ratio test to assess the significance of the car random effects (H0: sigma^2_psi=0)
### (not very reliable as we are in the edge of the parameter space)
nmodel <- lm(wear~tyre,data=tyres) # model without car_type as random effect
model <- lmer(wear~tyre+(1|car_type),data=tyres,REML=F) # model with car_type as random effect fitted using max. likelihood
logLik(model)
logLik(nmodel)
-2*(-109.4795+108.9102) ## Likelihood ration test
1-pchisq(1.1386,1) ## get the p-value

## Use parametric bootstrapping to test H0: sigma^2_psi=0
lrt <- c()	## create empty vector
rmodel=lmer(wear~tyre+(1|car_type),data=tyres)	## rand. effects model
lmodel=lm(wear~tyre,data=tyres)			## model under H0
LR_Value <- 2*(logLik(rmodel)-logLik(lmodel,REML=T)) ## The likelihood ratio value
LR_Value
simulate(lmodel) ## illustrate function to simulate response values from a fitted model (in this case the model under H0)
unlist(simulate(lmodel))
## Do the bootstrapping
for(i in 1:1000){
	y_tilde <- unlist(simulate(lmodel))
	lmod <- lm(y_tilde~tyre,data=tyres)
	rmod <- lmer(y_tilde~tyre+(1|car_type),data=tyres)
	lrt[i] <- 2*(logLik(rmod)-logLik(lmod,REML=TRUE))
}
plot(density(lrt)) ## smoothed histogram of the distribution of LR under H0
abline(v=LR_Value) ## Observed value of LR
mean(lrt > LR_Value) ## estimated p-value

### Fit a model with random slope for variable IQ
pupils[1:10,]
model = lmer(test~ses+IQ+(1+IQ|Class),data=pupils )
summary(model)
lmodel=lm(test~ses+IQ+Class,data=pupils ) ## fit model without random effects to compare the residual variances
summary(lmodel)
summary(model)
ranef(model) ## function to get the random effects
plot(1:131,ranef(model)$Class[,2]) ## plot the random effects for IQ to identify (potentially) "bad" classes, i.e. ones that hinder clever students.

