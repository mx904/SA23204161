## ----eval = FALSE-------------------------------------------------------------
#  library(MASS)
#  x<-mvrnorm(200,mu = rep(0,8),Sigma = diag(rep(1,8)))
#  eps<-rnorm(200,0,1)
#  beta<-matrix(c(.1,.2,.3,.3,.5,.9,.4,.4))
#  y<-x%*%beta+eps
#  g.index<-c(1,1,2,2,2,2,3,3)
#  p<-c(2,4,2)
#  fit<-G.nn.garrotte(x,y,g.index,p)
#  fit

## ----eval = FALSE-------------------------------------------------------------
#  L<-function(x,mu){(x-mu)^2}
#  x<-c(rnorm(200,0,1),rnorm(1800,2,1))
#  m<-180
#  theta<-mean(x[1:m])
#  alpha<-0.05
#  Tm<-10
#  d<-1
#  k<-SNSMS(L,x,theta,m,alpha,Tm,d)
#  plot(x,type="l")
#  abline(v=m+k,col="red")

## ----eval = FALSE-------------------------------------------------------------
#  library(MASS)
#  L<-function(x,mu){(x-mu)^2}
#  x<-cbind(t(mvrnorm(200,c(0,0),diag(c(1,1)))),t(mvrnorm(1800,c(1,2),diag(c(1,1)))))
#  m<-180
#  theta<-rowMeans(x[,1:m])
#  alpha<-0.05
#  Tm<-10
#  d<-2
#  SNSMS(L,x,theta,m,alpha,Tm,d)

## ----eval = FALSE-------------------------------------------------------------
#  L<-function(x,mu){(x-mu)^2}
#  x<-matrix(c(rnorm(200,0,0.1),rnorm(1800,2,0.1)),nrow = 1)
#  m<-180
#  theta<-mean(x[1:m])
#  alpha<-0.05
#  Tm<-10
#  d<-1
#  k<-SNSMS_Cpp(L,x,theta,m,alpha,Tm,d)
#  plot(x[1,],type="l")
#  abline(v=m+k,col="red")

## ----eval = FALSE-------------------------------------------------------------
#  library(MASS)
#  L<-function(x,mu){(x-mu)^2}
#  x<-cbind(t(mvrnorm(200,c(0,0),diag(c(1,1)))),t(mvrnorm(1800,c(1,2),diag(c(1,1)))))
#  m<-180
#  theta<-rowMeans(x[,1:m])
#  alpha<-0.05
#  Tm<-10
#  d<-2
#  SNSMS_Cpp(L,x,theta,m,alpha,Tm,d)

