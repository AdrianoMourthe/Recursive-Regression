source("recursiveRegression.R")
par(ask=FALSE)
n<-1
X<- cbind(Boston$lstat,Boston$rm)
N<-dim(X)[1]
y<-matrix(Boston$medv)
t<-numeric(3)
P<-500*diag(n+2)
mu<-1
for (i in 1:N){
  rls.step<-rls(c(1, X[i,]),y[i],t,P,mu)
  t<-rls.step[[1]]
  P<-rls.step[[2]]
}
t