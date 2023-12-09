#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericMatrix joint_density_cpp(double init_x=0,double init_y=0,
                                int a=5,int b=5,int n=20){
  Environment glo=Environment::global_env();
  Function f=glo["f"];
  int N=as<int>(glo["N"]);
  NumericMatrix res(N,3);
  res(0,0)=init_x;
  res(0,1)=init_y;
  res(0,2)=as<double>(f(res(0,0),res(0,1),a,b,n));
  for(int i=1;i<N;i++){
    res(i,0)=as<double>(Rcpp::rbinom(1,n,res(i-1,1)));
    res(i,1)=as<double>(Rcpp::rbeta(1,res(i-1,0)+a,n-res(i-1,0)+b));
    res(i,2)=as<double>(f(res(i,0),res(i,1),a,b,n));
  }
  return res;
}

