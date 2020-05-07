

th     = rnorm(10,0,1)
ip     = matrix(NA,nrow=4,ncol=4)
ip[,1] = rlnorm(4,0,.5)
ip[,2] = rnorm(4,0,1)
ip[,3] = 0
ip[,4] = 1

sim.d <- function(theta,ipar){
  
  y = matrix(nrow=length(theta),ncol=nrow(ipar))
  
  prob <- function(t,a,b,c,d){
    num = exp(a*(t-b))
    c + (d-c)*(num/(num+1))
  }
  
  for(i in 1:nrow(ipar)){
    p = prob(t=theta,a=ipar[i,1],b=ipar[i,2],c=ipar[i,3],d=ipar[i,4])
    u = runif(length(theta),0,1)
    y[,i] = ifelse(p<u,0,1)    
  }
  y
}


th     = rnorm(100,0,1)
ip     = matrix(NA,nrow=4,ncol=4)
ip[,1] = rlnorm(4,0,.5)
ip[,2] = rnorm(4,0,1)
ip[,3] = 0
ip[,4] = 1

y = sim.d(theta=th,ipar=ip)

x = c(1,0,1,0,1,0,1,0)

################################################

logL <- function(x,resp,theta){
  
  # resp : response vector
  # start: a vector of starting values
  
  # resp is a response vector for all individuals
  # for a single item
  
  # Quadrature points and weights
  
    # theta <- seq(-4,4,0.5)
    # 
    # gauss <- function(mu,sigma,theta) {
    #   weights <- exp(-0.5*((theta - mu)/sigma)^2)
    #   weights/sum(weights)
    # }
    # 
    # w = gauss(mu=0,sigma=1,theta)
    # 
    # 
   # Compute the loglikelihood of the whole response vector
    # for a given theta point
  
  x = matrix(x,nrow=length(x)/2,ncol=2,byrow=TRUE)
    
    LL <- function(theta,st,rvec){
      apar = st[1]
      bpar = st[2]
      cpar = 0
      dpar = 1
      num  = exp(apar*(theta-bpar))
      p1    = cpar + (dpar-cpar)*(num/(1+num))
      p2    = (p1^rvec)*((1-p1)^(1-rvec))
      sum(log(p2))
    }
    
    LLvec <- c()
    for(i in 1:ncol(resp)){
      LLvec[i]=LL(theta=th,st=x[i,],rvec=resp[,i])
    }
    
    sum(LLvec)
    
    # Compute the loglikelihood vector for 
    # nodes (quadrature points)
    
    # LLvec = c()
    # 
    # for(i in 1:length(theta)){
    #   LLvec[i] = LL(theta=theta[i])  
    # }
}


th     = rnorm(1000,0,1)
ip     = matrix(NA,nrow=4,ncol=4)
ip[,1] = rlnorm(4,0,.5)
ip[,2] = rnorm(4,0,1)
ip[,3] = 0
ip[,4] = 1

y = sim.d(theta=th,ipar=ip)

x = c(1,0,1,0,1,0,1,0)

 require(numDeriv)
 
 g = grad(func=logL,x = c(1,0,1,0,1,0,1,0),resp=y,theta=th)

 h = hessian(func=logL,x = c(1,0,1,0,1,0,1,0),resp=y,theta=th)

############################################################3

LLtrace <- c()
Gtrace  <- vector("list")
Htrace  <- vector("list")
par     <- vector("list")

par[[1]] =c(1,0,1,0,1,0,1,0)

LLtrace[1]   = logL(x=par[[1]],resp=y,theta=th)
Gtrace[[1]]  = grad(func=logL,x=par[[1]],resp=y,theta=th)
Htrace[[1]]  = hessian(func=logL,x=par[[1]],resp=y,theta=th)

iter = 2
while(iter < 1000){
  
  par[[iter]]= as.numeric(
    par[[iter-1]]-(solve(Htrace[[iter-1]])%*%as.matrix(Gtrace[[iter-1]])))
  
  LLtrace[iter]   = logL(x=par[[iter]],resp=y,theta=th)
  Gtrace[[iter]]  = grad(func=logL,x=par[[iter]],resp=y,theta=th)
  Htrace[[iter]]  = hessian(func=logL,x=par[[iter]],resp=y,theta=th)
  
  iter = iter+1
}






















