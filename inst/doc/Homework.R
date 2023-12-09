## -----------------------------------------------------------------------------
library(ggplot2)
library(knitr)
x<-seq(1,10,0.1)
y<-sin(x)
data<-data.frame(x,y)
ggplot(data = data, mapping = aes(x,y))+geom_line()+theme_light()+ggtitle("The figure of fuction y=sin(x).")
kable(head(data), align = "c", digits = c(1,4))

## -----------------------------------------------------------------------------
resources<-data.frame(book_id = 1:6, book_name = c("Statistical Computing with R","Advanced R","R for Beginners","Guidelines for Statistical Projects","Data Manupilation with R","R Graphics Cookbook"))

## -----------------------------------------------------------------------------
kable(resources, align = "c", caption = "Resources of class Statistical Computing")

## -----------------------------------------------------------------------------
data<-women
plot(data$weight,data$height,xlab = "weight", ylab = "height", main = "Scatter plot of height and weight.")
kable(data, align="c")
model<-lm(height~weight, data = data)
s<-summary(model)
print(s)

## -----------------------------------------------------------------------------
plot(data$weight,data$height,xlab = "weight", ylab = "height", main = "Scatter plot of height and weight.")
abline(coef = coef(s))

## -----------------------------------------------------------------------------
SEED<-20230918

## -----------------------------------------------------------------------------
my.sample<-function(data, size = length(data), prob = rep(1/length(data),length(data))){
  #Default: Assume "data" has "n" elements, and "n" samples will be drawn, whether the element will be drawn obeys uniform distribution.
  #The "replace" option is "TRUE".
  set.seed(SEED)
  cum.prob<-cumsum(prob)
  U<-runif(size)
  result<-data[findInterval(U,cum.prob)+1]
  return(result)
}
my.sample(1:5)
my.sample(6:10,size = 10)
my.sample(11:20,prob = seq(1,.1,-.1)/sum(seq(1,.1,-.1)))

## -----------------------------------------------------------------------------
n<-1000
set.seed(SEED)
U<-runif(n)
X1<-log(2*U[U<=0.5])
X2<--log(2*(1-U[U>0.5]))
X<-c(X1,X2)
hist(X,main = "Laplace distribution",xlim = c(-10,10),ylim = c(0,.5),prob = TRUE)
d<-seq(-10,10,.02)
lines(d,0.5*exp(1)^(-abs(d)),col = "red",lwd = 1)

## -----------------------------------------------------------------------------
set.seed(SEED)
n<-1000
iter<-count<-0
X.ar<-numeric(n)
while(count < n){
  iter<-iter+1
  U<-runif(1)
  Y<-runif(1,min = -10,max = 10)
  if(0.5*exp(1)^(-abs(Y))>U){
    count<-count+1
    X.ar[count]<-Y
  }
}
Ex3.2<-data.frame(Inverse_Transform = X,Acceptance_Rejection = X.ar)
library(ggplot2)
ggplot(data = Ex3.2)+xlab("Index")+
  geom_histogram(binwidth = 1,mapping = aes(x = Inverse_Transform,y = after_stat(density)),color = "red",fill = "#FDD7D7",alpha = 0.5)+
  geom_histogram(binwidth = 1,mapping = aes(x = Acceptance_Rejection,y = after_stat(density)),color = "green",fill = "#D6FED6",alpha = 0.5)+
  theme_light()+
  geom_line(mapping = aes(x = d[-1],y = 0.5*exp(1)^(-abs(d[-1]))),color = "black")

## -----------------------------------------------------------------------------
my.density.beta<-function(x,a = 3,b = 2){
  return(x^(a-1)*(1-x)^(b-1)/beta(a,b))
}
my.AR.beta<-function(n = 1000,a = 3,b = 2){
  count<-0
  X.beta<-numeric(n)
  set.seed(SEED)
  while(count < n){
    U<-runif(1)
    Y<-runif(1)
    if(my.density.beta(Y) > U){
      count<-count+1
      X.beta[count]<-Y
    }
  }
  return(X.beta)
}
X.beta<-my.AR.beta()
#Graph
ggplot()+theme_light()+
  geom_histogram(binwidth = 0.05,mapping = aes(x = X.beta,y = after_stat(density)),color = "red",fill = "#FDD7D7")+
  geom_line(mapping = aes(x = seq(0,1,.01),y = my.density.beta(seq(0,1,.01))))

## -----------------------------------------------------------------------------
my.kernel.RN<-function(n = 1e4){#Random Number
  set.seed(SEED)
  U1<-runif(n,-1,1)
  U2<-runif(n,-1,1)
  U3<-runif(n,-1,1)
  X.kernel<-numeric(n)
  for(i in 1:n){
    if(abs(U3[i])>=abs(U2[i]) && abs(U3[i])>=abs(U1[i])){
      X.kernel[i]<-U2[i]
    }else{
      X.kernel[i]<-U3[i]
    }
  }
  return(X.kernel)
}
X.kernel<-my.kernel.RN(1e5)
#Graph
ggplot()+theme_light()+
  geom_histogram(binwidth = 0.01,mapping = aes(x = X.kernel,y = after_stat(density)),color = "green",fill = "#D6FED6")

## -----------------------------------------------------------------------------
hist(X.kernel,prob = T)
lines(density(X.kernel),col = "red",lwd = 2)
index<-seq(-1,1,.001)
lines(index,0.75*(1-index^2),col = "blue", lwd = 2)

## -----------------------------------------------------------------------------
SEED<-20230925

## -----------------------------------------------------------------------------
K<-100
m<-1e6
s<-c(0.5,0.8,1)
num<-numeric(length(s))
for(i in 1:length(s)){
  l<-s[i]
  d<-1
  rho<-l/d
  pihat<-numeric(K)
  for(j in 1:K){
    X <- runif(m,0,d/2)
    Y <- runif(m,0,pi/2)
    pihat[j] <- 2*rho/mean(l/2*sin(Y)>X)
  }
  num[i]<-var(pihat)
}
data<-data.frame(t(num))
colnames(data)<-c('rho=0.5','rho=0.8','rho=1')
library(knitr)
kable(data, align = "c", digits = 8)
barplot(num,names.arg = c('rho=0.5','rho=0.8','rho=1'))

## -----------------------------------------------------------------------------
n<-1000
MCInt<-function(n = 1e4,method = c("simple","antithetic"),seed = 20230925){
  if(method == "simple"){
    set.seed(seed)
    U<-runif(n)
    theta<-mean(exp(U))
    return(theta)
  }else if(method == "antithetic"){
    set.seed(seed)
    U<-runif(n/2)
    theta<-mean(exp(U)+exp(1-U))/2
    return(theta)
  }else
    print("Wrong method!")
}
# simple MC vs antithetic variate
theta1 <- theta2 <- numeric(n)
for(i in 1:n){
  theta1[i]<-MCInt(method = "simple",seed = SEED+i)
  theta2[i]<-MCInt(method = "antithetic",seed = SEED+i)
}
var(theta1)
var(theta2)
(var(theta1)-var(theta2))/var(theta1)

## -----------------------------------------------------------------------------
rm(list=ls())
x<-seq(1,10,.01)[-1]
g<-function(x){
  gx<-x^2*exp(-x^2/2)/sqrt(2*pi)*(x>1)
  return(gx)
}
f1<-function(x){
  f1x<-dnorm(x,1,1)
  return(f1x)
}
f2<-function(x){
  f2x<-dgamma(x,2,2)
  return(f2x)
}
lg<-c("g(x)","f1(x)","f2(x)")
plot(x,g(x),lty = 1,lwd = 2,col = "red",type = "l",ylim = c(0,.5),ylab = "density",main = "Figure 1")
lines(x,f1(x),lty = 2,lwd = 2,col = "blue")
lines(x,f2(x),lty = 3,lwd = 2,col = "green")
legend("topright",legend = lg,col = c("red","blue","green"),lty = 1:3)

## ----eval=FALSE, include=FALSE------------------------------------------------
#  rm(list=ls())
#  g<-function(x){
#    gx<-x^2*exp(-x^2/2)/sqrt(2*pi)*(x>1)
#    return(gx)
#  }
#  f1<-function(x){
#    f1x<-dnorm(x,1,1)
#    return(f1x)
#  }
#  f2<-function(x){
#    f2x<-dgamma(x,2,2)
#    return(f2x)
#  }
#  SEED<-20231009
#  set.seed(SEED)
#  library(knitr)
#  # f1
#  n<-1e6
#  u<-rnorm(n,1,1)
#  fg<-g(u)/f1(u)
#  ghat<-Var<-numeric(2)
#  ghat[1]<-mean(fg)
#  Var[1]<-var(fg)
#  # f2
#  u<-rgamma(n,2,2)
#  fg<-g(u)/f2(u)
#  ghat[2]<-mean(fg)
#  Var[2]<-var(fg)
#  library(knitr)
#  data<-data.frame(matrix(c(ghat,Var),nrow = 2,byrow = T))
#  colnames(data)<-c("f1","f2")
#  rownames(data)<-c("ghat","Var")
#  kable(data,align = "ccc")

## -----------------------------------------------------------------------------
rm(list=ls())
g<-function(x){
  gx<-x^2*exp(-x^2/2)/sqrt(2*pi)*(x>1)
  return(gx)
}
n<-1e6
SEED<-20231009
u<-rnorm(n,1,1)
gf<-g(u)/dnorm(u,1,1)
ghat<-mean(gf)
Var<-var(gf)
cat("The estimate of theta is",round(ghat,6),"and its variance is",round(Var,6))

## -----------------------------------------------------------------------------
rm(list=ls())
library(knitr)
g<-function(x){ #for stratified sampling
  return(exp(-x)/(1+x^2)*(x>0)*(x<1))
}
g_si<-function(x,j){ #for stratified importance sampling
  return(exp(-x)/(1+x^2)*(x>(j-1)/5)*(x<j/5))
}
f<-function(x,j){
  return(exp(-x)/(exp(-(j-1)/5)-exp(-j/5)))
}
stra_sampling<-function(r,j,k){
  return(mean(g(runif(r,(j-1)/k,j/k))))
}
stra_im_sampling<-function(r,j){
  u<-runif(r)
  u<--log(exp(-(j-1)/5)-(exp(-(j-1)/5)-exp(-j/5))*u)
  return(mean(g_si(u,j)/f(u,j)))
}
data_print<-function(est){
  d<-as.data.frame(matrix(c(mean(est[,1]),mean(est[,2]),sd(est[,1]),sd(est[,2])),nrow = 2,byrow = T))
  colnames(d)<-c("Stratified sampling","Stratified importance sampling")
  rownames(d)<-c("theta_hat","sd")
  kable(d,align = "ccc")
}
M<-1000
k<-5
r<-M/k
N<-50
Theta1<-Theta2<-numeric(k)
est<-matrix(0,N,2)
SEED<-20231009
set.seed(SEED)
for(i in 1:N){
  for(j in 1:k){
    Theta1[j]<-stra_sampling(r,j,k)
    Theta2[j]<-stra_im_sampling(r,j)
  }
  est[i,]<-c(mean(Theta1),sum(Theta2))
}
data_print(est)

## -----------------------------------------------------------------------------
rm(list=ls())
iter<-1e5
n<-20
count<-0
SEED<-20231009
set.seed(SEED)
sample_generate<-function(n,df){
  x<-rchisq(n,df)
  sample_mean<-mean(x)
  se<-sd(x)/sqrt(n)
  return(c(sample_mean,se))
}
in_bound<-function(n,df,true_mean,alpha = 0.05){
  sample<-sample_generate(n,df)
  sample_mean<-sample[1]
  se<-sample[2]
  lower_bound<-sample_mean+qt(alpha/2,n-1)*se
  upper_bound<-sample_mean+qt(1-alpha/2,n-1)*se
  if(lower_bound<true_mean & upper_bound>true_mean) return(1)
  else return(0)
}
for(i in 1:iter){
  count<-count+in_bound(n,df = 2,true_mean = 2)
}
count/iter

## -----------------------------------------------------------------------------
rm(list=ls())
# Example 6.4
iter<-1e5
n<-20
count<-0
SEED<-20231009
set.seed(SEED)
sample_generate<-function(n,df){
  x<-rchisq(n,df)
  return(x)
}
in_bound<-function(n,df,alpha = 0.05){
  x<-sample_generate(n,df)
  upper_bound<-(n-1)*var(x)/qchisq(alpha, df=n-1)
  if(upper_bound>df^2) return(1)
  else return(0)
}
for(i in 1:iter){
  count<-count+in_bound(n,2)
}
count/iter

## -----------------------------------------------------------------------------
rm(list=ls())
alpha<-0.05 # nominal significance level
mu0<-1 # sample true mean
n<-30 # sample size
iter<-1e5 # total iterations
t1e<-0 # type 1 error counts
SEED<-20231009
set.seed(SEED)
sample_generate<-function(n,df){
  return(rchisq(n,df))
}
in_bound<-function(n,df,mu0,alpha){
  sample<-sample_generate(n,df)
  t_stat<-(mean(sample)-mu0)/(sd(sample)/sqrt(n)) 
  if(abs(t_stat)>qt(1-alpha/2,df = n-1)) return(1)
  else return(0)
}
for(i in 1:iter){
    t1e<-t1e+in_bound(n,df = 1,mu0,alpha)
}
Et1e<-t1e/iter
cat("Empirical Type I error rate is:", Et1e)

## -----------------------------------------------------------------------------
rm(list=ls())
alpha<-0.05 # nominal significance level
mu0<-1 # sample true mean
n<-30 # sample size
iter<-1e5 # total iterations
t1e<-0 # type 1 error counts
SEED<-20231009
set.seed(SEED)
sample_generate<-function(n,min,max){
  return(runif(n,min,max))
}
in_bound<-function(n,min,max,mu0,alpha){
  sample<-sample_generate(n,min,max)
  t_stat<-(mean(sample)-mu0)/(sd(sample)/sqrt(n))
  if(abs(t_stat)>qt(1-alpha/2,df = n-1)) return(1)
  else return(0)
}
for(i in 1:iter){
    t1e<-t1e+in_bound(n,min = 0,max = 2,mu0,alpha)
}
Et1e<-t1e/iter
cat("Empirical Type I error rate is:", Et1e)

## -----------------------------------------------------------------------------
rm(list=ls())
alpha<-0.05 # nominal significance level
mu0<-1 # sample true mean
n<-30 # sample size
iter<-1e5 # total iterations
t1e<-0 # type 1 error counts
SEED<-20231009
set.seed(SEED)
sample_generate<-function(n,rate){
  return(rexp(n,rate))
}
in_bound<-function(n,rate,mu0,alpha){
  sample<-sample_generate(n,rate)
  t_stat<-(mean(sample)-mu0)/(sd(sample)/sqrt(n))
  if(abs(t_stat)>qt(1-alpha/2,df = n-1)) return(1)
  else return(0)
}
for(i in 1:iter){
    t1e<-t1e+in_bound(n,rate = 1,mu0,alpha)
}
Et1e<-t1e/iter
cat("Empirical Type I error rate is:", Et1e)

## -----------------------------------------------------------------------------
rm(list = ls())
p_generate<-function(m,alt_prop){
  p1<-runif(m*(1-alt_prop))
  names(p1)<-rep("null",m*(1-alt_prop))
  p2<-rbeta(m*alt_prop,0.1,1)
  names(p2)<-rep("alte",m*alt_prop)
  return(sort(c(p1,p2)))
}
statistic_cal<-function(p){
  FWER<-1-(1-length(which(p<0.1 & names(p)=="null"))/1000)^1000
  FDR<-length(which(p<0.1 & names(p)=="null"))/length(which(p<0.1))
  TPR<-length(which(p<0.1 & names(p)=="alte"))/(1000*0.05)
  return(c(FWER,FDR,TPR))
}
M<-1000
alpha<-0.1
FWER<-FDR<-TPR<-matrix(0,nrow = M,ncol = 2)
set.seed(20231016)
for(i in 1:M){
  p<-p_generate(1000,0.05)
  adj.bonf<-p.adjust(p,method = "bonferroni")
  adj.BH<-p.adjust(p,method='BH')
  sc.bonf<-statistic_cal(adj.bonf)
  sc.BH<-statistic_cal(adj.BH)
  FWER[i,]<-c(sc.bonf[1],sc.BH[1])
  FDR[i,]<-c(sc.bonf[2],sc.BH[2])
  TPR[i,]<-c(sc.bonf[3],sc.BH[3])
}
data<-data.frame(FWER=c(mean(FWER[,1]),mean(FWER[,2])),FDR=c(mean(FDR[,1]),mean(FDR[,2])),TPR=c(mean(TPR[,1]),mean(TPR,2)))
row.names(data)<-c("Bonf","BH")
library(knitr)
kable(data,align = "ccc")

## -----------------------------------------------------------------------------
rm(list = ls())
lambda<-2
n<-c(5,10,20)
B<-1000
m<-1000
data_show<-matrix(0,nrow = 3,ncol = 4)
lambdahat<-lambdabias<-numeric(m)
samL<-numeric(m)
library(boot)
bootf<-function(x,i){
  return(1/mean(x[i]))
}
set.seed(20231016)
for(i in n){
  for(j in 1:m){
    x<-rexp(i,rate = lambda)
    obj<-boot(x,statistic = bootf,R = B)
    lambdabias[j]<-mean(obj$t)-obj$t0
    lambdahat[j]<-mean(obj$t)
  }
  data_show[which(n==i),]<-c(mean(lambdabias),lambda/(i-1),sd(lambdahat),lambda*i/(i-1)/sqrt(i-2))
}
d<-as.data.frame(data_show)
colnames(d)<-c("Bias.Boot","Bias.Theo","SE.Boot","SE.Theo")
rownames(d)<-c("n=5","n=10","n=20")
library(knitr)
kable(d,align = "cccc")

## -----------------------------------------------------------------------------
rm(list = ls())
library(bootstrap)
library(boot)
# cor.law<-cor(law$LSAT,law$GPA)
B<-1000
n<-nrow(law)
boot.cor<-function(x,i){
  return(cor(x[i,1],x[i,2]))
}
cor.hat<-se.boot<-numeric(B)
set.seed(20231016)
for(i in 1:B){
  x<-sample(1:n,replace = T)
  LSAT.b<-law$LSAT[x]
  GPA.b<-law$GPA[x]
  cor.hat[i]<-cor(LSAT.b,GPA.b)
  obj<-boot(data.frame(LSAT.b,GPA.b),statistic = boot.cor,R = B)
  se.boot[i]<-sd(obj$t)/sqrt(n)
}
t<-(cor.hat-mean(cor.hat))/mean(se.boot)
ci<-c(mean(cor.hat)-sd(cor.hat)/sqrt(n)*quantile(t,0.975),mean(cor.hat)-sd(cor.hat)/sqrt(n)*quantile(t,0.025))
names(ci)<-c("2.5%","97.5%")
ci
hist(t)

## -----------------------------------------------------------------------------
rm(list = ls())
library(boot)
x<-aircondit$hours
set.seed(20231023)
boot.mean<-function(x,i){
  return(mean(x[i]))
}
obj<-boot(x,statistic = boot.mean,R = 1000)
ci<-boot.ci(obj,type = c("norm","basic","perc","bca"))
ci

## -----------------------------------------------------------------------------
rm(list = ls())
library(bootstrap)
x<-scor
n<-nrow(x)
Sigma<-cor(x)
theta.hat<-eigen(Sigma)$values[1]/sum(eigen(Sigma)$values)
jackknife.bias.se<-function(x,n){
  theta.jack.hat<-numeric(n)
  for(i in 1:n){
    Sigma<-cor(x[-i,])
    theta.jack.hat[i]<-eigen(Sigma)$values[1]/sum(eigen(Sigma)$values)
  }
  return(theta.jack.hat)
}
theta.jack.hat<-jackknife.bias.se(x,n)
bias<-(n-1)*(mean(theta.jack.hat)-theta.hat)
se<-sqrt((n-1)/n*sum((theta.jack.hat-mean(theta.jack.hat))^2))
cat("bias=",bias,"\n","se=",se,sep = "")

## -----------------------------------------------------------------------------
rm(list = ls())
library(DAAG)
x<-ironslag
plot(x)

n<-nrow(x)
e1<-e2<-e3<-e4<-matrix(0,nrow = n*(n-1)/2,ncol = 2)

count<-1
for(i in 1:n){
  for(j in (i+1):n){
    del<-c(-i,-j)
    pre.x<-data.frame('chemical' = x$chemical[-del])
    train.x<-x[del,]
    
    model.1<-lm(magnetic ~ chemical,data = train.x)
    pre.1<-predict(model.1,newdata = pre.x)
    e1[count,]<-pre.1-x$magnetic[-del]
    
    model.2<-lm(magnetic ~ chemical + I(chemical^2),data = train.x)
    pre.2<-predict(model.2,newdata = pre.x)
    e2[count,]<-pre.2-x$magnetic[-del]
    
    model.3<-lm(log(magnetic) ~ chemical,data = train.x)
    pre.3<-predict(model.3,newdata = pre.x)
    e3[count,]<-exp(pre.3)-x$magnetic[-del]
    
    model.4<-lm(log(magnetic) ~ log(chemical),data = train.x)
    pre.4<-predict(model.4,newdata = pre.x)
    e4[count,]<-exp(pre.4)-x$magnetic[-del]
    
    count<-count+1 
  }
  if(i == n-1) break 
}
out<-c(mean(e1^2),mean(e2^2),mean(e3^2),mean(e4^2))
out

## ----warning=FALSE------------------------------------------------------------
rm(list = ls())
attach(chickwts)
x<-sort(as.vector(weight[feed == "soybean"]))
y<-sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)
z<-c(x,y)
n<-length(x)
m<-length(y)
k<-1:(n+m)
N<-999
W2<-numeric(N+1)
cal.W2<-function(z,n,m){
  x<-z[1:n]
  y<-z[-(1:n)]
  Fx<-ecdf(x)
  Fy<-ecdf(y)
  return((sum((Fx(x)-Fy(x))^2)+sum((Fx(y)-Fy(y))^2))*m*n/(m+n)^2)
}
W2[1]<-cal.W2(z,n,m)
set.seed(20231030)
for(i in 1:N){
  index<-sample(k,size = n,replace = F)
  x0<-z[index]
  y0<-z[-index]
  W2[i+1]<-cal.W2(c(x0,y0),n,m)
}
p<-mean(W2>W2[1])
p

## -----------------------------------------------------------------------------
rm(list = ls())
n1<-20
n2<-30
mu1<-mu2<-0
sigma1<-1
sigma2<-1
count5.test<-function(x1,x2){
  x1<-x1-mean(x1)
  x2<-x2-mean(x2)
  out1<-sum(x1<min(x2))+sum(x1>max(x2))
  out2<-sum(x2<min(x1))+sum(x2>max(x1))
  return(max(out1,out2)>5)
}
permutation.count5.test<-function(x1,x2,N){
  z<-c(x1,x2)
  n1<-length(x1)
  n2<-length(x2)
  counter<-numeric(N)
  for(i in 1:N){
    index<-sample(1:(n1+n2),size = n1,replace = F)
    x0<-z[index]
    y0<-z[-index]
    counter[i]<-count5.test(x0,y0)
  }
  t0<-count5.test(x1,x2)
  return(mean(c(t0,counter)))
}
set.seed(20231030)
pct<-permutation.count5.test(rnorm(n1,mu1,sigma1),rnorm(n2,mu2,sigma2),N = 9999)
pct

## -----------------------------------------------------------------------------
rm(list = ls())
calcu.a<-function(N,b,f0){
  x1<-rpois(N,1)
  x2<-rexp(N,1)
  x3<-rbinom(N,1,0.5)
  a.hat<--log(1/f0-1)-b%*%c(mean(x1),mean(x2),mean(x3))
  return(a.hat)
}

## -----------------------------------------------------------------------------
N<-1e6
b<-c(0,1,-1)
f0<-c(0.1,0.01,0.001,0.0001)
a<-numeric(4)
for(i in 1:4){
  a[i]<-calcu.a(N,b,f0[i])
}
f0_a<-data.frame(f0=f0,a=a)
knitr::kable(f0_a,align = "cc")

## -----------------------------------------------------------------------------
N<-1e4
f0<-seq(1e-4,1,1e-2)
a<-numeric(length(f0))
for(i in 1:length(f0)){
  a[i]<-calcu.a(N,b,f0[i])
}
plot(-log(f0),a,type = "l")

## -----------------------------------------------------------------------------
rm(list = ls())
f<-function(x){
  0.5*exp(-abs(x))
}
rw.Metroplis<-function(N,x0,sigma){
  x<-numeric(N)
  x[1]<-x0
  U<-runif(N)
  k<-0
  for(i in 2:N){
    y<-rnorm(1,x[i-1],sigma)
    if(U[i] < f(y)/f(x[i-1])){
      x[i]<-y
      k<-k+1
    }
    else{
      x[i]<-x[i-1]
    }
  }
  return(list(x=x,k=k))
}
N<-3000
x0<-0
sigma<-c(0.5,1,2,5)
set.seed(12345)
rw1<-rw.Metroplis(N,x0,sigma[1])
rw2<-rw.Metroplis(N,x0,sigma[2])
rw3<-rw.Metroplis(N,x0,sigma[3])
rw4<-rw.Metroplis(N,x0,sigma[4])
reject_p<-data.frame(sigma=sigma,No.accept=c(rw1$k,rw2$k,rw3$k,rw4$k),P_accept=c(rw1$k,rw2$k,rw3$k,rw4$k)/N)
knitr::kable(reject_p,align = 'ccc')
# par(mfrow = c(2,2))
plot(rw1$x,type = 'l',ylab = 'X')
plot(rw2$x,type = 'l',ylab = 'X')
plot(rw3$x,type = 'l',ylab = 'X')
plot(rw4$x,type = 'l',ylab = 'X')

## -----------------------------------------------------------------------------
rm(list = ls())
Gibbs<-function(N,x,mu = c(0,0),sigma = c(1,1),rho = 0.9){
  X<-matrix(0,nrow = N+1,ncol = 2)
  X[1,]<-x
  for(i in 2:(N+1)){
    X[i,1]<-rnorm(1,mu[1]+rho*sigma[1]/sigma[2]*(X[i-1,2]-mu[2]),(1-rho^2)*sigma[1]^2)
    X[i,2]<-rnorm(1,mu[2]+rho*sigma[2]/sigma[1]*(X[i,1]-mu[1]),(1-rho^2)*sigma[2]^2)
  }
  return(X[-1,])
}
burn<-1000
N<-3000
x0<-c(0,0)
data<-Gibbs(N,x0)[-(1:burn),]
plot(data[,1],type = "l",col = 1,lwd = 1,ylab = "Xt, Yt",main = "Burn = 1000")
lines(data[,2],col = 2)
legend("topright",c("Xt","Yt"),col = 1:2,cex = 0.8,lty = 1)

## -----------------------------------------------------------------------------
reg<-data.frame(data)
colnames(reg)<-c("Xt","Yt")
model<-lm(Yt ~ Xt,data = reg)
plot(model,which=c(1,2))

## -----------------------------------------------------------------------------
rm(list = ls())
f<-function(x,sigma){
  if(any(x<0))
    return(0)
  stopifnot(sigma>0)
  return(x/sigma^2*exp(-x^2/2/sigma^2))
}
MH.sampler<-function(N,x0,sigma){
  X<-numeric(N)
  X[1]<-x0
  U<-runif(N)
  for(i in 2:N){
    Y<-rchisq(1,df = X[i-1])
    r1<-f(Y,sigma)*dchisq(X[i-1],df=Y)
    r2<-f(X[i-1],sigma)*dchisq(Y,df=X[i-1])
    alpha<-r1/r2
    if(U[i] <= alpha){
      X[i]<-Y
    }else{
      X[i]<-X[i-1]
    }
  }
  return(X)
}
GR.statistic<-function(phi){
  N<-ncol(phi)
  k<-nrow(phi)
  Bn<-N/(k-1)*sum((rowMeans(phi)-mean(phi))^2)
  Wn<-1/k/N*sum((phi-rowMeans(phi))^2)
  Var.phi<-(N-1)/N*Wn+Bn/N
  R<-Var.phi/Wn
  return(R)
}
burn<-2000
N<-15000
sigma<-4
x0<-c(1,4,9,16)
k<-length(x0)
X<-matrix(0,k,N)
set.seed(12345)
for(i in 1:k){
  X[i,]<-MH.sampler(N,x0[i],sigma)
}
phi<-t(apply(X,1,cumsum))
for(i in 1:k)
  phi[i,]<-phi[i,]/(1:N)
GR.hat<-numeric(N)
for(j in (burn+1):N){
  GR.hat[j]<-GR.statistic(phi[,1:j])
}
# par(mfrow = c(2,2))
for(i in 1:k){
  plot(phi[i,(burn+1):N],type = "l")
}
# par(mfrow = c(1,1))
plot((burn+1):N,GR.hat[-(1:burn)],type = "l",ylab = "GR",xlab = "index")
abline(h = 1.2,col = "red",lty = 2)

## -----------------------------------------------------------------------------
library(coda)
XX<-X[,-(1:burn)]
x1<-as.mcmc(XX[1,])
x2<-as.mcmc(XX[2,])
x3<-as.mcmc(XX[3,])
x4<-as.mcmc(XX[4,])
M<-mcmc.list(x1,x2,x3,x4)
gelman.diag(M)
gelman.plot(M)

## ----warning=FALSE------------------------------------------------------------
rm(list = ls())
library(stats4)
# MLE
u <- c(11,8,27,13,16,0,23,10,24,2)
v <- c(12,9,28,14,17,1,24,11,25,3)
mlogL <- function(lambda = 1){
  return(-sum(log(exp(-lambda*u)-exp(-lambda*v))))
}
fit <- mle(mlogL)
fit@coef

## -----------------------------------------------------------------------------
rm(list = ls())
# EM
u <- c(11,8,27,13,16,0,23,10,24,2)
v <- c(12,9,28,14,17,1,24,11,25,3)
n <- length(u)
lam0 <- .1
lam1 <- .2
eps <- 1e-8
k <- 0
repeat{
  lam1 <- n / (sum((u*exp(-lam0*u)-v*exp(-lam0*v))/(exp(-lam0*u)-exp(-lam0*v)))+n / lam0)
  if(abs(lam0 - lam1) < eps){
    break
  }
  lam0 <- lam1
  k <- k+1
}
lam1

## -----------------------------------------------------------------------------
rm(list = ls())
game <- function(A) {
  ori.A <- A
  A <- (A - min(A))/max(A)
  m <- nrow(A)
  n <- ncol(A)
  it <- n^3
  a <- c(rep(0, m), 1)
  A1 <- -cbind(t(A), rep(-1, n))
  b1 <- rep(0, n)
  A3 <- t(as.matrix(c(rep(1, m), 0)))
  b3 <- 1
  sx <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3, maxi=TRUE, n.iter=it)
  a <- c(rep(0, n), 1)
  A1 <- cbind(A, rep(-1, m))
  b1 <- rep(0, m)
  A3 <- t(as.matrix(c(rep(1, n), 0)))
  b3 <- 1
  sy <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3,
                maxi=FALSE, n.iter=it)
  solution <- list("A" = A * max(ori.A) + min(ori.A),
               "x" = sx$soln[1:m],
               "y" = sy$soln[1:n],
               "v" = sx$soln[m+1] * max(ori.A) + min(ori.A))
  solution
}
A <- matrix(c( 0,-2,-2,3,0,0,4,0,0,
               2,0,0,0,-3,-3,4,0,0,
               2,0,0,3,0,0,0,-4,-4,
               -3,0,-3,0,4,0,0,5,0,
               0,3,0,-4,0,-4,0,5,0,
               0,3,0,0,4,0,-5,0,-5,
               -4,-4,0,0,0,5,0,0,6,
               0,0,4,-5,-5,0,0,0,6,
               0,0,4,0,0,5,-6,-6,0), nrow = 9, ncol =  9)
library(boot)
s.A <- game(A)
s.B <- game(A + 2)
action.B <- round(cbind(s.B$x, s.B$y), 6)
colnames(action.B) <- c("Game B, Player 1","Game B, Player 2")
knitr::kable(action.B, align = "ccc")
cat("It is the extreme points (11.15) of the original game A\n")
cat("The value of game A is",s.A$v,"\nThe value of game B is",s.B$v)

## ----eval=FALSE---------------------------------------------------------------
#  scale01 <- function(x) {
#    rng <- range(x, na.rm = TRUE)
#    (x - rng[1]) / (rng[2] - rng[1])
#  }

## -----------------------------------------------------------------------------
rm(list = ls())
x<-list(c(1,2),c(1,2,3,4))
unlist(x)
as.vector(x)

## -----------------------------------------------------------------------------
rm(list = ls())
x<-as.vector(c(1,2,3,4))
is.vector(x)
dim(x)

y<-vector(mode = "logical",length = 5)
is.vector(y)
dim(y)

## -----------------------------------------------------------------------------
rm(list = ls())
x<-matrix(1:20,nrow = 4,ncol = 5)
is.matrix(x)
is.array(x)

## -----------------------------------------------------------------------------
rm(list = ls())
x<-data.frame(a=c(1,2,3,4),b=c("A","B","C","D"),c=c(TRUE,TRUE,FALSE,FALSE))
c(class(x[,1]),class(x[,2]),class(x[,3]))
y<-as.matrix(x)
y
class(y)
typeof(y)

## -----------------------------------------------------------------------------
rm(list = ls())
x<-data.frame(a=numeric(0),b=logical(0))
c(nrow(x),ncol(x))
y<-data.frame()
c(nrow(y),ncol(y))

## -----------------------------------------------------------------------------
rm(list = ls())
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
x<-as.data.frame(matrix(1:20,4,5))
apply(x,2,scale01)
y<-data.frame(a=c(1,2,3,4),b=c("A","B","C","D"),c=c(TRUE,TRUE,FALSE,FALSE))
index<-sapply(y,is.numeric)
apply(as.matrix(y[,which(index==1)]),2,scale01)

## -----------------------------------------------------------------------------
rm(list = ls())
x<-as.data.frame(matrix(1:20,4,5))
vapply(x,sd,FUN.VALUE = c(sd=1))

## -----------------------------------------------------------------------------
rm(list = ls())
x<-data.frame(a=c(1,2,3,4),b=c("A","B","C","D"),c=c(0,0,5,10))
index<-vapply(x,is.numeric,FUN.VALUE = c(I=1))
vapply(x[,which(index==1)],sd,FUN.VALUE = c(sd=1))

## -----------------------------------------------------------------------------
rm(list = ls())
N<-1e+4
# R function
f<-function(x,y,a,b,n){
  choose(n,x)*y^(x+a-1)*(1-y)^(n-x+b-1)
}
joint.density.R<-function(init.x=0,init.y=0,a=5,b=5,n=20){
  z<-x<-y<-numeric(N)
  x[1]<-init.x
  y[1]<-init.y
  z[1]<-f(x[1],y[1],a,b,n)
  for(i in 2:N){
    x[i]<-rbinom(1,n,y[i-1])
    y[i]<-rbeta(1,x[i-1]+a,n-x[i-1]+b)
    z[i]<-f(x[i],y[i],a,b,n)
  }
  return(data.frame(x,y,z))
}
data<-joint.density.R()[-(1:(N/2)),]
head(data)
# library(plotly)
# p<-plot_ly(data,x=~x,y=~y,z=~z) %>% add_markers(color = ~z)
# p

## -----------------------------------------------------------------------------
# Rcpp function
library(Rcpp)
sourceCpp("joint.density.cpp")
data<-as.data.frame(joint_density_cpp())[-(1:(N/2)),]
colnames(data)<-c("x","y","z")
head(data)
# p<-plot_ly(data,x=~x,y=~y,z=~z) %>% add_markers(color = ~z)
# p

## -----------------------------------------------------------------------------
library(microbenchmark)
ts<-microbenchmark(joint.R=joint.density.R(),joint.cpp=joint_density_cpp())
summary(ts)[,c(1,3,5,6)]

