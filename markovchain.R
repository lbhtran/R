library(markovchain)
urnstate <- c("0","1","2","3","4")

byRow <- T

Tmatrix <- matrix(data= c(0,1,0,0,0,1/4,0,3/4,0,0,0,2/4,0,2/4,0,0,0,3/4,0,1/4,0,0,0,1,0), byrow = T, nrow=5, dimnames = list(urnstate,urnstate))

urnmc <- new("markovchain", states = urnstate, byrow=T, transitionMatrix = Tmatrix, name = "Urn1")

urnmc



fractions(steadyStates(urnmc))


plot(ctime[1,], type="b")
lines(ctime[2,])
lines(ctime[3,])
lines(ctime[4,])
lines(ctime[5,])

## Urn1 probability state MC
### initial state
time0 <- t(as.matrix(c(0,0,0,0,1)))
t = 20

A <- matrix(t(time0),5)

for (i in 1:t) {
	timei <- time0 * urnmc**i
	A <- cbind(A,t(timei))
}


plot(A[1,], type="l", ylim=c(0,1))
for (i in 2:5) {
	lines(A[i,])
}

lines(A[1,])

 
#auto plot
inistate <- diag(5)
t <- 20
for (i in 1:5) {
	time0 <- inistate[,i]
	A <- matrix(time0,5)
	for (i in 1:t) {
		timei <- time0 * urnmc**i
		A <- cbind(A,t(timei))
	}
	plot(A[1,], type="l", ylim=c(0,1))
	for (i in 2:5) {
		lines(A[i,])
	}	
}

color <- c("royalblue","")

par(mfrow=c(2,3))


plot(A[1,], type="l")
lines(A[2,], col="blue")
lines(A[3,], col="red")
lines(A[4,], col="green")
lines(A[5,], col="yellow")


### time average

time0 <- as.matrix(c(0,0,0,0,1))
t = 2

time0 <- t(as.matrix(c(0,1,0,0,0)))
time1 <- time0 * urnmc**1
time2 <- time0 * urnmc**2
time3 <- time0 * urnmc**3


A <- matrix(time0,5)

Sn <- (1/2)*(time0+time1)

t=50

Sn <- matrix(time0,5)
sumi <- time0


for (i in 1:t) {
	timei <- time0 * urnmc**i
	sumi <- sumi+timei
	Sn <- cbind(Sn, (1/(i+1))*t(sumi))
}
Sn[,10001]

steadyStates(urnmc)


plot(Sn[1,], type="l", ylim=c(0,1))
for (i in 2:5) {
	lines(Sn[i,])
}


library(markovchain)
urnstate <- c("0","1","2","3","4")

byRow <- T

Tmatrix <- matrix(data= c(0.5,0.5,0,0,0,1/4,0,3/4,0,0,0,2/4,0,2/4,0,0,0,3/4,0,1/4,0,0,0,1,0), byrow = T, nrow=5, dimnames = list(urnstate,urnstate))

urnmc2 <- new("markovchain", states = urnstate, byrow=T, transitionMatrix = Tmatrix, name = "Urn1")

urnmc2

### initial state
time0 <- t(as.matrix(c(0,0,0,0,1)))
t = 2000

A <- matrix(t(time0),5)

for (i in 1:t) {
	timei <- time0 * urnmc2**i
	A <- cbind(A,t(timei))
}

A

plot(A[1,], type="l", ylim=c(0,1))
for (i in 2:5) {
	lines(A[i,])
}

## average time

t=50

Sn <- matrix(time0,5)
sumi <- time0


for (i in 1:t) {
	timei <- time0 * urnmc2**i
	sumi <- sumi+timei
	Sn <- cbind(Sn, (1/(i+1))*t(sumi))
}
Sn[,51]

steadyStates(urnmc2)


plot(Sn[1,], type="l", ylim=c(0,1))
for (i in 2:5) {
	lines(Sn[i,])
}





### Gambler's ruin

gamblerRuinMarkovChain <- function(moneyMax, prob = 0.5) {
	require(matlab)
	matr <- zeros(moneyMax + 1)
	states <- as.character(seq(from = 0, to = moneyMax, by = 1))
	rownames(matr) = states; colnames(matr) = states
	matr[1,1] = 1;matr[moneyMax + 1,moneyMax + 1] = 1
	for(i in 2:moneyMax) {
		matr[i,i-1] = 1-prob;matr[i,i+1] = prob
	}
	out <- new("markovchain", transitionMatrix = matr, name = paste("Gambler ruin", moneyMax, "dim", sep = " "))
	return(out)
}

mcGR4 <- gamblerRuinMarkovChain(moneyMax = 4, prob = 0.5)

