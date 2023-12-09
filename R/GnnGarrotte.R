#' @title Group nn Garrotte
#' @description Group non-negative Garrotte algorithm using R
#' @param x design matrix for linear regression
#' @param y response variable
#' @param g.index a vector containing group information for each variable in \code{x}
#' @param p a vector containing the number of variables in the same group
#' @param normalized a logical value, default is "TRUE". If it is TRUE, each column of \code{x} will be scaled
#' @param gram a logical value, default is "TRUE". If it is TRUE, the gram-Schmidit orthonormalization will be applied for each group
#' @return a list containing the origin data \code{x}, \code{y}, \code{g.index}, \code{p}, and the d-matrix \code{d}, estimated parameters \code{betas}, solution path \code{path}
#' @examples
#' \dontrun{
#'   library(MASS)
#'   x<-mvrnorm(200,mu = rep(0,8),Sigma = diag(rep(1,8)))
#'   eps<-rnorm(200,0,1)
#'   beta<-matrix(c(.1,.2,.3,.3,.5,.9,.4,.4))
#'   y<-x%*%beta+eps
#'   g.index<-c(1,1,2,2,2,2,3,3)
#'   p<-c(2,4,2)
#'   fit<-G.nn.garrotte(x,y,g.index,p)
#'   fit
#' }
#' @export
G.nn.garrotte<-function(x,y,g.index,p,normalized = TRUE,gram = TRUE){
  if (gram){
    for (col in unique(g.index)){
      if (sum(g.index == col) > 1){
        x[,g.index == col]<-pracma::gramSchmidt(x[,g.index == col])$Q
      }
    }
  }
  if (normalized == TRUE){
    non.zeros.cols = apply(x!=0,2,sum) != 0
    x[,non.zeros.cols] = scale(x[,non.zeros.cols])
    x[is.na(x)] = 0
    # y <- scale(y)
  }
  fit<-stats::lm(y~x)
  b.LS<-as.matrix(fit$coefficients[-1])
  z<-matrix(nrow = nrow(y),ncol = length(p))
  for(i in unique(g.index)){
    z[,i]<-x[,g.index==i,drop = F]%*%b.LS[g.index==i,]
  }
  d.rec<-matrix(nrow = length(p))
  k<-1
  d<-numeric(length(p))
  d.rec[,k]<-d
  r<-matrix(y)
  alpha<-0
  temp<-numeric(length(p))
  for(i in unique(g.index)){
    temp[i]<-t(z[,i,drop = F])%*%r/p[i]
  }
  Ck<-which(temp==max(temp))
  path.order<-numeric(length(unique(g.index)))
  path.order[1]<-Ck
  gCk<-numeric(length(p))
  while(alpha!=1){
    gCk[Ck]<-MASS::ginv(t(z[,Ck])%*%z[,Ck])%*%t(z[,Ck])%*%r
    alpha.j<-numeric(length(p))
    for(j in unique(g.index[-which(g.index %in% Ck)])){
      alpha.j.root<-function(alpha.j,j,Z,Ck){
        t(z[,j])%*%(r-alpha.j*z%*%matrix(gCk))/p[j] - t(z[,Ck[1]])%*%(r-alpha.j*z%*%matrix(gCk))/p[Ck[1]]}
      alpha.j[j]<-stats::uniroot(alpha.j.root,c(-10,10),j,z,Ck)$root
    }
    for(j in Ck){
      alpha.j[j]<-min(1,-d[j]/gCk[j])
    }
    if(all(alpha.j<=0) || min(alpha.j[which(alpha.j>0)])>1){ 
      alpha<-1
    }else{
      alpha<-min(alpha.j[which(alpha.j>0)])
      j.star<-which(alpha.j==alpha)
    }
    d<-d+alpha*gCk
    if(all(j.star %in% Ck)){
      Ck<-Ck[-which(Ck %in%  j.star)]
    }else {
      Ck<-c(Ck,j.star)
      path.order[k+1]<-j.star
    }
    r<-y-z%*%d
    k<-k+1
    d.rec<-cbind(d.rec,d)
  }
  b.rec<-matrix(nrow = length(b.LS),ncol = ncol(d.rec))
  for(i in 1:nrow(d.rec)){
    b.rec[which(g.index==i),]<-matrix(b.LS[which(g.index==i),])%*%d.rec[i,]}
  return(list(X = x,Y = y,g.index = g.index,p = p,d = d.rec,betas = b.rec,path = path.order))
}