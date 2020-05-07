

require(mirt)

###################################################################
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

###################################################################

th     = rnorm(1000,0,1)
ip     = matrix(NA,nrow=4,ncol=4)
ip[,1] = rlnorm(4,0,.5)
ip[,2] = rnorm(4,0,1)
ip[,3] = 0
ip[,4] = 1

y = as.data.frame(sim.d(theta=th,ipar=ip))

###################################################################

est.ipar <- vector('list')

x = seq(100,1000,by=5)

for(i in 1:length(x)){
  #mod <- 'F = 1-4 
  #        CONSTRAIN = (1-4,a1)'
  
  fit = mirt(y[1:x[i],],1,itemtype='2PL')
  est.ipar[[i]] <- coef(fit,simplify=TRUE,IRT=TRUE)$items
}


##################################################################

library(ggplot2)
library(animation)
theme_set(theme_bw())

item = 3

itempar = c(x[1],est.ipar[[1]][item,])

for(i in 2:length(est.ipar)){
  itempar = rbind(itempar,c(x[i],est.ipar[[i]][item,]))
}

colnames(itempar)[1] = 'N'


t = seq(-3,3,.01)

bigd = expand.grid(x,t)
bigd$p1 = NA
bigd$p2 = NA

for(i in 1:length(x)){
  temp = which(bigd[,1]==x[i])
  num  = exp(itempar[i,2]*(bigd[temp,2]-itempar[i,3]))
  bigd[temp,]$p1 = itempar[i,4]+(itempar[i,5]-itempar[i,4])*(num/(1+num))
  num  = exp(ip[item,1]*(bigd[temp,2]-ip[item,2]))
  bigd[temp,]$p2 = ip[item,3]+(ip[item,4]-ip[item,3])*(num/(1+num))
}


oopt <- ani.options(nmax = 500)

sub1 = cbind(bigd[,1:3],'True')
colnames(sub1) <- c('n','theta','p','type')
sub2 = cbind(bigd[,c(1,2,4)],'Estimated')
colnames(sub2) <- c('n','theta','p','type')
sub = rbind(sub1,sub2)

p <- ggplot(sub,aes(x=theta,y=p,col=type))+
  geom_line()+
  theme(legend.title = element_blank())+
  labs(subtitle = paste0("(a = ",round(true.ip[1],2),
                         ", b = ",round(true.ip[2],2),
                         ", c = ",round(true.ip[3],2),
                         ", d = ",round(true.ip[4],2),")"))+
  ylab("Probability")+
  xlab(expression(theta)) 

p + transition_reveal(n) +
    ease_aes('linear')



irtrace <- function(bigd){
  
  x <- unique(bigd[,1])
  
  for (i in 1:length(x)) {

    # plot(th,prob,ylim=c(0,1),xlim=c(-1.5,1.5),type='l',lwd=2,
    #      main=paste0('Sample Size = ',x[i],"\n\n",
    #                  "(a = ",round(true.ip[1],2),
    #                  ", b = ",round(true.ip[2],2),
    #                  ", c = ",round(true.ip[3],2),
    #                  ", d = ",round(true.ip[4],2),")"
    #                  )
    #      )
    # 
    # temp = which(bigd[,1]==x[i])
    # points(bigd[temp,]$theta,bigd[temp,]$p,type='l',col='blue',lwd=2)
    # 
    # legend('topleft',
    #        c("True","Estimated"),
    #        lwd=c(2,2),
    #        col=c('black','blue'))
    # 
    ani.pause()
  }
}

irtrace(bigd)

saveGIF(irtrace(bigd))

























