rm(list=ls())
##################3.2
s=751+594+1213+1126+819
n=5
dtheta=rgamma(1000, shape = n, rate = s)
plot(dtheta)
###############
dlambda=1/dtheta
plot(dlambda)
############
mean(dlambda>1000)
##############3.8
LIKE = function(lambda) pexp(100,1/lambda)^3*dexp(100,1/lambda)*
  (pexp(300,1/lambda)-pexp(100,1/lambda))^3*
  dexp(300,1/lambda)*(1-pexp(300,1/lambda))^4

post <- function(lambda) {
  post1=LIKE(lambda) / lambda
  post2=post1/sum(post1)
  return( post2)
}
######
curve(post,from=50,to=1000)
############
mu=sum(x*post(x))
var=(sum((x-mu)^2*post(x)))^0.5
var2=(sum(x^2*post(x))-mu^2)^0.5
mu
var
var2
#################
sum(post(x)[x>300&x<500])




