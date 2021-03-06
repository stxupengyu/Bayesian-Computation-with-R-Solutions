rm(list=ls())
mu=seq(20, 70, by=10)
prior = c(.1,.15,.25,.25,.15,.1)
plot(mu,prior,type="l",col=4,lwd=4)
#####################################
y=c(38.6,42.4,57.5,40.5,51.7,67.1,33.4,60.9,64.1,40.1,40.7,6.4)
ybar=mean(y)
plot(y,type="l",col=4,lwd=3)
########################################
sigma=10
n=length(mu)
like=rep(0,n)
for(i in 1:n){
  like[i]=exp(1/2*-n/(sigma)^2*(mu[i]-ybar)^2)
}
plot(mu,like,type="l",col=4,lwd=3)
#####################################
post=rep(0,n)
for(i in 1:n){
  post[i]=prior[i]*like[i]/sum(prior*like)
}
plot(mu,post,type="l",col=4,lwd=3)
####################################

disc=cbind(mu,post)
prob=0.8
discint(disc,prob)
