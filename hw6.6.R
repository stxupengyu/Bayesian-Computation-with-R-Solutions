#6.6
#a
rm(list=ls())
library(LearnBayes)
data=c(15,11,14,17,5,11,10,4,8,10,7,9,11,3,6,1,1,4)
f=function(beta,data){
  beta0=beta[1]
  beta1=beta[2]
  i=1:18
  y=data
  sum((beta0+beta1*i)*y-exp(beta0+beta1*i))
}
sim=10000
burnin=2000
start1=c(4.5,0.1)
start2=c(127/8,-7/8)
fit=laplace(f,start2,data)
fit

mycontour(f,c(2.2,3.4,-0.15,0),data,xlab="beta0",ylab="beta1")

proposal=list(var=fit$var,scale=2)
fit2=rwmetrop(f,proposal,start1,sim,data)
fit2$accept

mycontour(f,c(2.2,3.4,-0.15,0),data,xlab="beta0",ylab="beta1")
points(fit2$par[burnin:sim, 1], fit2$par[burnin:sim, 2])

post.means=apply(fit2$par,2,mean)
post.sds=apply(fit2$par,2,sd)

modal.sds=sqrt(diag(fit$var))
cbind(c(fit$mode), modal.sds)
cbind(post.means, post.sds)

library(coda)
library(lattice)
dimnames(fit2$par)[[2]]=c("beta0", "beta1")
xyplot(mcmc(fit2$par[-c(1:burnin), ]), col = "black")

par(mfrow = c(2, 1))
autocorr.plot(mcmc(fit2$par[-c(1:burnin), ]), auto.layout = FALSE)

mean(fit2$par[burnin:sim,2])
sd(fit2$par[burnin:sim,2])

#b

proposal2=list(var = fit$var, mu = t(fit$mode))
fit3=indepmetrop(f, proposal2, start2, sim, data)
fit3$accept

mycontour(f,c(2.2,3.4,-0.15,0),data,xlab="beta0",ylab="beta1")
points(fit3$par[burnin:sim, 1], fit3$par[burnin:sim, 2])

mean(fit3$par[burnin:sim,2])
sd(fit3$par[burnin:sim,2])
#c
fit4=gibbs(f, start2, sim, c(0.3, 0.05), data)
fit4$accept
mycontour(f,c(2.2,3.4,-0.15,0),data,xlab="beta0",ylab="beta1")
points(fit4$par[burnin:sim, 1], fit4$par[burnin:sim, 2])

mean(fit4$par[burnin:sim,2])
sd(fit4$par[burnin:sim,2])
#comparison
plot(density(rnorm(sim,fit$mode[2],sqrt(fit$var[2,2]))),
     type="l",col=4,lwd=2,lty=1,main="Simulation Comparison",
     xlab="beta1",ylab="posterior density",ylim=c(0,30))
lines(density(fit2$par[burnin:sim,2]),type="l",col=3,lwd=1)
lines(density(fit3$par[burnin:sim,2]),type="l",col=2,lwd=1)
lines(density(fit4$par[burnin:sim,2]),type="l",col=1,lwd=1)
legend("topleft",c("normal","rw","inde","gibbs"),
      lty=1,col=c(4,3,2,1))


r12=rnorm(sim,fit$mode[2],sqrt(fit$var[2,2]))
r22=fit2$par[burnin:sim,2]
r32=fit3$par[burnin:sim,2]
r42=fit4$par[burnin:sim,2]

t12=c(quantile(r12,0.05),quantile(r12,0.5),quantile(r12,0.95))
t22=c(quantile(r22,0.05),quantile(r22,0.5),quantile(r22,0.95))
t32=c(quantile(r32,0.05),quantile(r32,0.5),quantile(r32,0.95))
t42=c(quantile(r42,0.05),quantile(r42,0.5),quantile(r42,0.95))
t12
t22
t32
t42

r12=rnorm(sim,fit$mode[1],sqrt(fit$var[1,1]))
r22=fit2$par[burnin:sim,1]
r32=fit3$par[burnin:sim,1]
r42=fit4$par[burnin:sim,1]

t11=c(quantile(r12,0.05),quantile(r12,0.5),quantile(r12,0.95))
t21=c(quantile(r22,0.05),quantile(r22,0.5),quantile(r22,0.95))
t31=c(quantile(r32,0.05),quantile(r32,0.5),quantile(r32,0.95))
t41=c(quantile(r42,0.05),quantile(r42,0.5),quantile(r42,0.95))
t11
t21
t31
t41

#d
library(GA)
f_ga=function(beta0,beta1){
  data=c(15,11,14,17,5,11,10,4,8,10,7,9,11,3,6,1,1,4)
  i=1:18
  y=data
  sum((beta0+beta1*i)*y-exp(beta0+beta1*i))
}

GA=ga(type = "real-valued", 
      fitness =  function(x) f_ga(x[1], x[2]),
      lower = c(1, -2), upper = c(8, 1), 
      popSize = 50, maxiter = 1000, run = 100)
summary(GA)
plot(GA)

