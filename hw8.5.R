#8.5a
rm(list=ls())
data=matrix(c(17,90,51,45,265,143,31,139,71),c(3,3),byrow = T)
data
chisq.test(data)
#b
a=matrix(rep(1,9),c(3,3))
a
ctable(data, a)


#c
log.K=seq(6,12)
compute.log.BF=function(log.K)log(bfindep(data,exp(log.K),100000)$bf)
log.BF=sapply(log.K,compute.log.BF)
BF=exp(log.BF)
round(data.frame(log.K,log.BF,BF),2)

#d

