###############1.2 simulating data for regression and likelihood ##########

n <- 10
beta <- 0.8
tau <- 1
y<- runif(n,10,20)
hist(y)
consump <- rnorm(n, beta*y, 1/sqrt(tau))
hist(consump)

#by ols
b <- sum(consump*y)/sum(y*y)
#or equiv lm(consump~y - 1)$coefficients
betavalues <- seq(0.75,0.85, length.out = 100)
plot(betavalues, dnorm(betavalues, b, 1/sqrt(tau*sum(y*y))), type="l")

plot(consump, y)
abline(lm(consump ~ y - 1))

########### Ejemplo autoregresivo ##########
rm(list = ls())
n <- 51
rho<- 0.9
tau<-1
y<-rep(0,n)
y[1]<-0
for(i in 2:n){
  y[i]<-rho*y[i-1]+rnorm(1, 0, 1/sqrt(tau))
}
plot(y, type="l") #note, no realization of random walk

#by ols
r=sum(y[2:n]*y[1:(n-1)])/sum(y[1:(n-1)]*y[1:(n-1)])
#or equiv lm(y[2:n]~y[1:(n-1)]-1)$coefficients

rhovalues <- seq(0.4,1.2, length.out = 100)
plot(rhovalues, dnorm(rhovalues, r, 1/sqrt(tau*sum(y[1:(n-1)]*y[1:(n-1)]))), type="l")

######binary data #############
rm(list = ls())
n <- 50
beta <- 0.05
x <- runif(n,10,20)
y <- rbinom(n,1,pnorm(beta*x))
plot(x,y)

#likelihood
betavalues <- seq(-0.2,0.2, length.out = 10000)

likelihood_single_beta <- function(beta_i,x,y){
  a<-pnorm(beta_i*x)^y*(1-pnorm(beta_i*x))^(1-y)
  return(prod(a))
}

like_of_multiple_beta<-sapply(betavalues, likelihood_single_beta, x=x,y=y)
plot(betavalues,like_of_multiple_beta, type="l")
#beta of max likelihood
betavalues[which.max(like_of_multiple_beta)]


