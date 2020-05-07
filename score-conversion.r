
score.table <- function(mirtobject,slope,int){
  
  #################################################
  # Input
  # 
  # mirtobject --> model fit object from mirt function
  # slope      --> scaling slope coef from technical manual
  # int        --> scaling intercept coef from technical manual
  #   
  # Output
  # 
  # a table with score conversion information
  #################################################
  
  d  = mirtobject@Data$data
  ni = ncol(d)
    
  tab     <- as.data.frame(matrix(nrow=ni+1,ncol=3))
  tab[,1] <- 0:ni

  for(i in 0:ni){
    
    tab[i+1,2:3] <- fscores(mirtobject,
                                         method="EAPsum",
                                         response.pattern = c(rep(1,i),rep(0,ni-i)),
                                         append_response.pattern = FALSE)[1,]
  }

  colnames(tab) <- c("TotalScore","Theta","SE")

  tab$ScaleScore = tab[,2]*slope+int
  tab$SEM = tab[,3]*slope

  tab$Frequency <- NA
  
  d <- as.matrix(d)
  
  if(length(which(is.na(d)==TRUE))!=0){
    d[which(is.na(d)==TRUE)] = 0
  }
  
  d <- as.data.frame(d)
  
  for(i in 0:ni){
    tab[i+1,]$Frequency <- length(which(rowSums(d)==i))
  }

  tab$CumFreq <- cumsum(tab$Frequency)
  
  tab

}

##############################################################

# sim.d <- function(theta,ipar){
#   
#   y = matrix(nrow=length(theta),ncol=nrow(ipar))
#   
#   prob <- function(t,a,b,c,d){
#     num = exp(a*(t-b))
#     c + (d-c)*(num/(num+1))
#   }
#   
#   for(i in 1:nrow(ipar)){
#     p = prob(t=theta,a=ipar[i,1],b=ipar[i,2],c=ipar[i,3],d=ipar[i,4])
#     u = runif(length(theta),0,1)
#     y[,i] = ifelse(p<u,0,1)    
#   }
#   y
# }

################################################################
# 
# N = 250 # sample size
# k = 10   # number of items
# 
# th     = rnorm(N,0,1)
# ip     = matrix(NA,nrow=k,ncol=4)
# ip[,1] = rlnorm(k,0,.5)
# ip[,2] = rnorm(k,0,1)
# ip[,3] = 0
# ip[,4] = 1
# 
# y = sim.d(theta=th,ipar=ip)
# 
# require(mirt)
# 
# fit = mirt(data=as.data.frame(y),1,itemtype = '2PL')
# 
# score.table(fit,slope=50,int=500)













