rm(list=ls())
lambda=seq(0.5, 3, by=.5)
prior = c(.1,.2,.3,.2,.15,.05)
plot(lambda,prior,type="l",col=4,lwd=4)
##########
n=length(lambda)
g=function(x){
  for(i in 1:n){
    if(x==lambda[i]){
      result=prior[i]
    }
  }
  return(result)
}

post_prob=function(x,t,y){
  result=g(x)*exp(-t*x)*(t*x)^y
}

post=rep(0,n)
for(i in 1:n){
  post[i]=post_prob(lambda[i],6,12)
}

sum_post=sum(post)
for(i in 1:n){
  post[i]=post[i]/sum_post
}


plot(lambda,post,type="l",col=4,lwd=4)
######################
post=rep(0,n)
for(i in 1:n){
  post[i]=post_prob(lambda[i],7,0)
}

sum_post=sum(post)
for(i in 1:n){
  post[i]=post[i]/sum_post
}

plot(lambda,post,type="l",col=4,lwd=4)

con_prob=function(lambda){
  return(exp(-7*lambda))
}

pred_prob=con_prob(lambda)

plot(lambda,pred_prob,type="l",col=4,lwd=4)

last_prob=pred_prob*post
sum_last_prob=sum(last_prob)
sum_last_prob
