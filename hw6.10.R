#6.10a
rm(list=ls())
set.seed(11)
library(LearnBayes)
data=c(13,52,6,40,10,7,66,10,10,14,16,4,65,5,11,10,15,5,76,
       56,88,24,51,4,40,8,18,5,16,50,40,1,36,5,10,91,18,1,
       18,6,1,23,15,18,12,12,17,3)
g=function(theta,data){
  lambda=theta[1]
  mu=theta[2]
  sigma=exp(theta[3])
  y=data
  sum(log(dnorm((y^lambda-1)/lambda,mu,sigma))+(lambda-1)*log(y))
}
#b
start=c(0.1,3,0.5)
laplacefit=laplace(g,start,data)
laplacefit$mode
#c

proposal1=list(var=laplacefit$var,scale=1.7)
fitrw=rwmetrop(g,proposal1,start,10000,data)
fitrw$accept
pararw=fitrw$par
pararw[,3]=exp(pararw[,3])


proposal2=list(var = laplacefit$var, mu = t(laplacefit$mode))
fitinde=indepmetrop(g, proposal2, start, 10000, data)
fitinde$accept
parainde=fitinde$par
parainde[,3]=exp(parainde[,3])

fitgibbs=gibbs(g,start,10000,scale =c(0.15,1.5,0.55), data)
fitgibbs$accept
paragibbs=fitgibbs$par
paragibbs[,3]=exp(paragibbs[,3])

lambda=pararw[,1]
plot(density(lambda),main="lambda")
mu=pararw[,2]
plot(density(mu),main="mu")
sigma=pararw[,3]
plot(density(sigma),main="sigma")
#d
apply(pararw,2,quantile,c(.05,.95))
apply(parainde,2,quantile,c(.05,.95))
apply(paragibbs,2,quantile,c(.05,.95))


