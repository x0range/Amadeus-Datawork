rm(list = ls())
setwd("/Users/mainuser/Library/Mobile Documents/com~apple~CloudDocs/2-Data/ORBIS data")
setwd("/Users/francois/Library/Mobile Documents/com~apple~CloudDocs/2-Data/ORBIS data")

load("Data_France.Rda")
load("Data_Italy.Rda")
load("Data_Spain.Rda")
load("Data_Germany.Rda")

mydf<-as.data.frame(rbind(
  Data_Spain[[1]],
  Data_Germany[[1]],
  Data_Italy[[1]],
  Data_France[[1]]))
rm(Data_Spain,Data_Germany,Data_Italy,Data_France)
head(mydf)
LP<-mydf$LP
length(LP)==5420962
rm(mydf)

# length(LP)/4340125

# sample size
NN<-c(10,20,50,100,200,500,1000,2000,5000,
      10000,20000,50000,100000,200000,500000,1000000,2000000,length(LP))
nN<-length(NN)

# number of simulations of each sample size
nsim=1000
library(parallel) # for parallel computations
NCORES<-4 # number of cores used for parallel computation
##############################
# Main loop
##############################
t0<-Sys.time() # record current time
mysd<-array(dim=c(nN,nsim))
for(i in 1:nN){
  if(i==nN){
    mysd[i,]<-rep(sd(LP),nsim)
  }else{
    ### parallelized version
    myl<-mclapply(as.list(1:nsim),
                  function(x){sd(sample(LP,size=NN[i],replace=F))},
                  mc.cores = NCORES)
    mysd[i,]<-unlist(myl)
    ### standard version
    # for(sim in 1:nsim){
    #   mysd[i,sim]<-sd(sample(LP,size=NN[i],replace=F))
    # }
  }
  print(paste("N=",NN[i]))
}
print(Sys.time()-t0) 
# nsim=1000: 9.19min on i5 1.6GHz

##############################
# Compute mean and quantiles
##############################
mm<-apply(mysd,1,mean)
qhigh<-apply(mysd,1,quantile,0.95)
qlow<-apply(mysd,1,quantile,0.05)

## Compute intercept
# install.packages("StableEstim")
library("StableEstim")
mc<-McCullochParametersEstim(LP)
ALPHA=mc[1]
theo<-NN^((1/ALPHA)-(1/2))
xx<-replicate(nsim,rstable(NN[1],mc[1],mc[2],mc[3],mc[4]))
xxsd<-apply(xx,2,sd)
intercept<-mean(xxsd)
theo<-theo*(intercept/theo[1])

pdf(file="Levy_sd_scaling.pdf",height=4,width=6)
par(mar=c(4,4,1,1))
par(las=1)
plot(mm~NN,log="xy",ylim=range(qhigh,qlow),
     col=1,pch=16,xaxt="n",yaxt="n",cex=1.3,
     ylab="sample standard deviation",xlab="sample size, N")
axis(2,at=c(10,20,50,100,200,500,1000,2000,5000),c(10,20,50,100,200,500,1000,2000,5000))
axis(1,at=10^(1:7),parse(text=paste("10^", seq(1:7))))
lines(mm~NN,lwd=3,col=1)
lines(qlow~NN,col=1)
lines(qhigh~NN,col=1)
points(qlow~NN,col=1,cex=0.5,pch=17)
points(qhigh~NN,col=1,cex=0.5,pch=17)
lines(theo~NN,col=4,lwd=2)
legend("topleft",col=c(1,1,1,4),lwd=c(3,1,1,2),pch=c(16,17,17,NA),bty="n",
    pt.cex=c(1.3,0.5,0.5,NA),   
    legend=c("average","5th percentile","95th percentile",expression(N^(1/hat(alpha)-1/2))) )
dev.off()

# Compare Levy alpha with alpha derived from OLS fit of the scaling
scal<-summary(lm(log(mm)~log(NN)))$coef[2,1]-2*summary(lm(log(mm)~log(NN)))$coef[2,2]
alphahat2<-1/(scal+(1/2))
x<-round(c(alphahat2,ALPHA),3)
names(x)<-c("alpha (scaling OLS slope)","alpha (Levy fit)")
print(x)
#alpha (scaling OLS slope)          alpha (Levy fit) 
#1.318                     1.331 
