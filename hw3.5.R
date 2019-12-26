rm(list=ls())
#########################3.5
#a
2*min(1-pbinom(8, 20, 0.2),pbinom(8, 20, 0.2))

#b
p=0.2
prob=0.5
para=c(1, 4)
data=c(8, 12)
library(LearnBayes)
pbetat(p, prob, para, data)$post
#c
pbetat(p, prob, c(0.5, 2), data)$post
pbetat(p, prob, c(2, 8), data)$post
pbetat(p, prob, c(8, 32), data)$post
#d

#########################3.7
rm(list=ls())
library(LearnBayes)
#a
f=function(lambda) {
  0.5*dgamma(lambda, 1.5, 1000) + 
    0.5*dgamma(lambda, 7, 1000)
}
g1=function(lambda){dgamma(lambda, 1.5, 1000)}
g2=function(lambda){dgamma(lambda, 7, 1000)}
curve(f, add = F,xlim = c(0, 0.015),ylim = c(0, 400),col=2,lwd = 3, xlab = "Lambda", ylab = "DENSITY")

#b
prior=rbind(c(1.5, 1000), c(7, 1000))
prob=c(0.5, 0.5)
data=list(y = 4, t=1767)
poisson.gamma.mix(prob, prior, data)$prob

#c
para=poisson.gamma.mix(prob, prior, data)$prob
gammapara=poisson.gamma.mix(prob, prior, data)$gammapar
f1 = function(lambda) {
  para[1]*dgamma(lambda, gammapara[1,1], gammapara[1,2]) + 
    para[2]*dgamma(lambda, gammapara[2,1], gammapara[2,2])
}
curve(f1, add = T, col = 3)

#d
set.seed(1)
inde = rbinom(10000, 1, para[1])
gg1 =rgamma(10000, gammapara[1,1], gammapara[1,2])
gg2 = rgamma(10000, gammapara[2,1], gammapara[2,2])
gg = inde * gg1 + (1-inde) * gg2
mean(g>0.005)
#e

curve(g1, add = T, col = 4)#first expert

curve(g2, add = T, col = 5)



