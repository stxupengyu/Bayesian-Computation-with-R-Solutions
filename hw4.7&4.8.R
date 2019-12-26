rm(list=ls())
#########################4.8
#a
y=c(0,0,6,12,7,9,3)
n=c(2,7,14,26,13,14,3)
x=c(16,18,20,22,24,26,28)
data = cbind(x, n, y)
response = cbind(y, n - y)
results = glm(response ~ x, family = binomial)
summary(results)

library(LearnBayes)
beta.select(list(p=.25,x=.15),list(p=.75,x=.35))

beta.select(list(p=.25,x=.75),list(p=.75,x=.95))

#b

prior=rbind(c(18,8.51,2.21),c(26,5.12,4.25))
data.new=rbind(data, prior)
mycontour(logisticpost,c(-15,0,0,0.8),data.new,xlab="beta0",ylab="beta1",col=3)

#c
s0=simcontour(logisticpost,c(-15,0,0,0.8),data.new,1000)
points(s0)

#d
p=1-(1/(exp(s0$x+s0$y*20)+1))
quantile(p,c(.05,.95))



#4.7
#a
rm(list=ls())
library(LearnBayes)
y=c(12.2,.9,.8,5.3,2,1.2,1.2,1,.3,1.8,3.1,2.8)
gamma.sampling.post=function(theta,y)sum(dgamma(y,shape=theta[1],scale=theta[2],log=TRUE))
mycontour(gamma.sampling.post,c(0.05,3.5,0.5,30), y)
s=simcontour(gamma.sampling.post,c(0.05,3.5,0.5,30),y,1000)
points(s)

quantile(s$x*s$y,c(.05,.95))


#b
n=length(y)
gamma.sampling.post_1=function(theta_1,y)
{
  a=theta_1[1]
  b=theta_1[2]
  (a*n-2)*log(b)-n*log(gamma(a))+(a-1)*sum(log(y))-b*sum(y)
}
mycontour(gamma.sampling.post_1,c(0.000001,3.3,0.000001,1.47), y,xlab="a",ylab="b=1/lambda")
s1=simcontour(gamma.sampling.post_1,c(0.000001,3.3,0.000001,1.47),y,1000)
points(s1)
miu_1=s1$x/s1$y
e2=quantile(miu_1,c(.05,.95))


#c
n=length(y)
gamma.sampling.post_2=function(theta_2,y)
{
  a=theta_2[1]
  miu_2=theta_2[2]
  -a*n*log(miu_2)+(a*n-1)*log(a)-n*log(gamma(a))+(a-1)*sum(log(y))-a*sum(y)/miu_2
}
mycontour(gamma.sampling.post_2,c(0.1,3.8,0.5,16), y,xlab="a",ylab="miu=a*lambda")
s2=simcontour(gamma.sampling.post_2,c(0.1,3.8,0.5,16),y,1000)
points(s2)
miu_2=s2$y
e3=quantile(miu_2,c(.05,.95))


#d

e1_u=numeric(50)  # a)题方法区间上界
e1_l=numeric(50)   # a)题方法区间下界
e2_u=numeric(50)  # b)题方法区间上界
e2_l=numeric(50)   # b)题方法区间下界
e3_u=numeric(50)   # c)题方法区间上界
e3_l=numeric(50)    # c)题方法区间下界
for(i in 1:50)
{
  s1=simcontour(gamma.sampling.post,c(0.05,3.5,0.5,30),y,1000)
  miu1=s1$x*s1$y
  e1=quantile(miu1,c(.05,.95))
  e1_u[i]=e1[1]
  e1_l[i]=e1[2]
  s2=simcontour(gamma.sampling.post_1,c(0.000001,3.3,0.000001,1.47),y,1000)
  miu2=s2$x/s2$y
  e2=quantile(miu2,c(.05,.95))
  e2_u[i]=e2[1]
  e2_l[i]=e2[2]
  s3=simcontour(gamma.sampling.post_2,c(0.1,3.8,0.5,16),y,1000)
  miu3=s3$y
  e3=quantile(miu3,c(.05,.95))
  e3_u[i]=e3[1]
  e3_l[i]=e3[2]
}
boxplot(e1_u,e2_u,e3_u,ylab="mu")

boxplot(e1_l,e2_l,e3_l,ylab="lambda")






