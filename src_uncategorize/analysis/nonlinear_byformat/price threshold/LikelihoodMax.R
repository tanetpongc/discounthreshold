# Likelihood maximazation and estimation 
# Rickard Sandberg, Sapporo, 2019-08-07


x <- rpois(1000,2) #Generating data: X~Po(mu=2)

poisson.lik <- function(mu,y){
  n<-length(y)
logl <- sum(y)*log(mu)-n*mu
return(-logl)
}

est<-optim(1.0, poisson.lik, method="BFGS", hessian=TRUE, y=x)

OI<-solve(est$hessian)

par.est<-est$par

se<-sqrt(diag(OI))

t.stat=par.est/se
t.stat



make_copula <- function(x) {
  if (length(unique(x))==1) return(as.numeric(rep(NA, length(x))))
  return(ifelse(ecdf(x)(x)==1, qnorm(1-.0000001), qnorm(ecdf(x)(x))))
}

# (1) Define your model mt. Just write the expression for your model (everything on rhs except the error term) - this is a scalar expression. Label the unknown parameters to be estimated in a suitable way
# (2) Define et = yt - mt
# (3) Write the LogL_t expression for et (assuming et~N(0, sigma2)): logL_t = -0.5 ln (2*pi) -  et^2 /(2 * sigma2)
# (4) Finally, the function to maximize: logL = sum {from 1 to T} ( LogL_t )
# (5) return (-LogL)


# We use data of Loka, bottled of water in supermarket that have an evidence of full model

df_bottledwater <- fread("../../../../data/df_bottledwater_super.csv")
df_bottledwater_loka <- df_bottledwater[brand == "Loka",]

#Define function
function(c,a0,agh,bgh,alh,blh,agc,bgc,alc,blc,k1,k2,k3,phi1,phi2,phi3,sigma2){
  mt = c + (a0 + (agh/(1+exp(100()))))
  et = dlogtotalvolume - mt
  #for t = 1 to t = 152
  llt = -0.5log(2*pi)-(et^2/2*sigma2) #from t1 to t152
  #sum t+t+1
  
  return(-llt)
}