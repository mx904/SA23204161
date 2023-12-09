// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
using namespace Rcpp;
//' @import Rcpp
//' @import RcppEigen
//' @title SNSMS for RCpp version
//' @description Self-normalized sequential change-point detection using RCpp
//' @param L a d-dimension object function
//' @param x a d-dimension time series data, expressed as a matrix
//' @param theta parameter estimates for the first m datas
//' @param m the number of observed data
//' @param alpha significance level
//' @param Tm the ratio of the monitoring horizon to the size of the training sample
//' @param d the dimension of data
//' @return a random sample of size \code{k}
//' @examples
//' \dontrun{
//'   L<-function(x,mu){(x-mu)^2}
//'   x<-matrix(c(rnorm(200,0,0.1),rnorm(1800,2,0.1)),nrow = 1)
//'   m<-180
//'   theta<-mean(x[1:m])
//'   alpha<-0.05
//'   Tm<-10
//'   d<-1
//'   k<-SNSMS_Cpp(L,x,theta,m,alpha,Tm,d)
//'   plot(x,type="l")
//'   abline(v=m+k,col="red")
//' }
//' @examples
//' \dontrun{
//'   library(MASS)
//'   L<-function(x,mu){(x-mu)^2}
//'   x<-cbind(t(mvrnorm(200,c(0,0),diag(c(1,1)))),t(mvrnorm(1800,c(1,2),diag(c(1,1)))))
//'   m<-180
//'   theta<-rowMeans(x[,1:m])
//'   alpha<-0.05
//'   Tm<-10
//'   d<-2
//'   k<-SNSMS_Cpp(L,x,theta,m,alpha,Tm,d)
//' }
//' @export
// [[Rcpp::export]]
int SNSMS_Cpp(Rcpp::Function L,NumericMatrix x,NumericVector theta,
              int m,double alpha,int Tm,int d) {
  int Tm1, alpha1, d1;
  switch(Tm){
  case 1:
    Tm1=1;
    break;
  case 2:
    Tm1=2;
    break;
  case 10:
    Tm1=3;
    break;
  default:
    Tm1=0;
  }
  if(Tm >=100){Tm1 = 4;}
  switch(d){
  case 1:
    d1 = 1;
    break;
  case 2:
    d1 = 2;
    break;
  case 3:
    d1 = 3;
    break;
  default:
    d1 = 0;
  }
  if(alpha == 0.05){alpha1 = 1;}else if(alpha == 0.1){alpha1 = 2;}else{alpha1 = 0;}
  if(Tm1 == 0 || alpha1 == 0 || d1 == 0){
    Rcpp::stop("error value");
  }
  NumericVector c_mat = NumericVector::create(33.1,22.6,44.2,30.2,60.5,41.3,66.2,45.2,69.3,50.8,92.3,67.7,126.4,92.7,138.4,101.4,112,85.2,149.5,113.8);
  c_mat.push_back(204.2);
  c_mat.push_back(155.5);
  c_mat.push_back(223.6);
  c_mat.push_back(170.3);
  c_mat.attr("dim") = Dimension(2,12);
  double c=c_mat(alpha1-1,d1*Tm1-1);
  double Mm;
  if(x.rows() == 1){
    double Dm=0;
    double Sm=0;
    for(int t=0;t<m;t++){
      double temp=0;
      for(int j=0;j<t;j++){
        temp=temp + as<double>(L(x.column(j),theta));
      }
      Dm=Dm + temp*temp;
    }
    Dm=Dm / pow(m,2);
    for(int k=m-1;k<m*Tm;k++){
      for(int t=m;t<m+k;t++){
        Sm=Sm + as<double>(L(x.column(t),theta));
      }
      Mm=Sm*Sm/Dm;
      Mm/=m*pow(1+k/m,2);
      if(Mm > c){
        return k;
      }
    }
    return (m*Tm+1);
  }else{
    Eigen::MatrixXd Dm(x.rows(),x.rows());
    Eigen::VectorXd Sm(x.rows());
    for(int t=0;t<m;t++){
      Eigen::VectorXd temp(x.rows());
      for(int j=0;j<t;j++){
        temp=temp + as<Eigen::VectorXd>(L(x.column(j),theta));
      }
      Dm=Dm + temp*temp.transpose();
    }
    Dm=Dm / pow(m,2);
    for(int k=m-1;k<m*Tm;k++){
      for(int t=m;t<m+k;t++){
        Sm=Sm + as<Eigen::VectorXd>(L(x.column(t),theta));
      }
      Mm=Sm.transpose()*Dm.inverse()*Sm;
      Mm/=m*pow(1+k/m,2);
      if(Mm > c){
        return k;
      }
    }
    return (m*Tm+1);
  }
}

