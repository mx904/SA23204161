#' @title SNSMS
#' @description Self-normalized sequential change-point detection
#' @param L a d-dimension object function
#' @param x a d-dimension time series data, expressed as a matrix
#' @param theta parameter estimates for the first m datas
#' @param m the number of observed data
#' @param alpha significance level
#' @param Tm the ratio of the monitoring horizon to the size of the training sample
#' @param d the dimension of data
#' @return a random sample of size \code{k}
#' @examples
#' \dontrun{
#'     L<-function(x,mu){(x-mu)^2}
#'     x<-c(rnorm(200,0,1),rnorm(1800,2,1))
#'     m<-180
#'     theta<-mean(x[1:m])
#'     alpha<-0.05
#'     Tm<-10
#'     d<-1
#'     k<-SNSMS(L,x,theta,m,alpha,Tm,d)
#'     plot(x,type="l")
#'     abline(v=m+k,col="red")
#' }
#' @examples
#' \dontrun{
#'   library(MASS)
#'   L<-function(x,mu){(x-mu)^2}
#'   x<-cbind(t(mvrnorm(200,c(0,0),diag(c(1,1)))),t(mvrnorm(1800,c(1,2),diag(c(1,1)))))
#'   m<-180
#'   theta<-rowMeans(x[,1:m])
#'   alpha<-0.05
#'   Tm<-10
#'   d<-2
#'   k<-SNSMS(L,x,theta,m,alpha,Tm,d)
#' }
#' @export
SNSMS<-function(L,x,theta,m,alpha,Tm,d){
  # db.c<-array(c(33.1,22.6,44.2,30.2,60.5,41.3,66.2,45.2,69.3,50.8,92.3,67.7,126.4,92.7,138.4,101.4,112,85.2,149.5,113.8,204.2,155.5,223.6,170.3))
  # dim(db.c)<-c(2,4,3)
  if(alpha==0.05){alpha1<-1}else if(alpha==0.1){alpha1<-2} else stop("wrong alpha value")
  if(Tm==1){Tm1<-1}else if(Tm==2){Tm1<-2} else if(Tm==10){Tm1<-3} else if(Tm>100){Tm1<-4} else stop("wrong T value")
  if(d==1){d1<-1}else if(d==2){d1<-2}else if(d==3){d1<-3}else stop("wrong d value")
  utils::data(db.c)
  db.c<-db.c[alpha1,Tm1,d1]
  Dm<-0
  Sm<-0
  for(t in 1:m){
    temp<-0
    for(j in 1:t){
      temp<-temp+L(x[j],theta)
    }
    Dm<-Dm+temp%*%t(temp)
  }
  Dm<-Dm/(m^2)
  for(k in m:(m*Tm)){
    for(t in (m+1):(m+k)){
      Sm<-Sm+L(x[t],theta)
    }
    Mm<-t(Sm)%*%solve(Dm)%*%Sm/m/(1+k/m)^2
    if(Mm>db.c){
      return(k)
    }
  }
  return(m*Tm+1)
}
