### Vasicek estimation

library(remotes)
install_version("SMFI5", "1.0")
library(SMFI5)

sim.vasicek


#==================
# Vasicek interest-rate model parameters
#==================

r0 <- 0.04 # 2.55 # initial interest rate
sigma <- 0.0195 # 0.365 # r volatility parameter
beta <- 0.04 # 2.55 # long-term level of interest rate
alpha <- 0.0395 # 0.5 # mean-reversion speed kappa

#=======================
# simulation parameters
#=======================

npy <- 1 # 365 or 12
T <- 30 # 30 # number of simulation in years
NYinR <- 20 # number of years in retirement (payout phase)
h <- 1/npy # time step 1/12 or 1/360
n <- T/h # number of periods

msim <- 10000
set.seed(123) # for reproducibility

# simulate Vasicek short-term rate

rt <- matrix(r0,msim,n)

# r <- matrix(r0,n+1,msim)

for(i in 1:msim){
  rt[i,]<- sim.vasicek(alpha, beta, sigma, r0, n-1, h)
}

timeline <- seq(0,n*h, h)

# head(rt[,1:10])



#==================================
# plot of interest rate simulations
#==================================

if (write) {
  pdf(file.path(outdir,paste0("VasicekRateSimulation",msim,"sim",T,"y.pdf"))) #VasicekRateSimulationUnbounded
  # png(file.path(outdir,paste0("VasicekRateSimulation",msim,"sim",T,"y.png"))) # , res=5
  plot(timeline,rt[,1], ylim=range(rt,beta), type="l", col="mediumorchid2")
  for(j in 2:msim){
    lines(timeline,rt[,j], col=colors()[floor(runif(1,1,657))] )
  }
  abline( h = beta, col = "red")
  text(0, beta*1.2,paste("long term interest level: beta =",beta),col="red",adj=0)
  title(main="Simulation of Vasicek interest rate") # , col.main="black", font.main=4
  dev.off()
}


### Modern tontine

TPXG<-function(x,t,m,b){exp(exp((x-m)/b)*(1-exp(t/b)))}

### Binomial Simulation of Gompertz

# Parameters are set.
x<-65; m<-90; b<-10; GL0<-1000; TH<-30; N<-10000
# Placeholders are created.
GLIVE<-matrix(nrow=N,ncol=TH)
GDEAD<-matrix(nrow=N,ncol=TH)
# Loop through N simulations.
for (i in 1:N){
  # Simulate deaths in year 1.
  GDEAD[i,1]<-rbinom(1,GL0,1-TPXG(x,1,m,b))
  # Subtract deaths from GL0 to get survivors .
  GLIVE[i,1]<-GL0-GDEAD[i,1]
  # Loop through remaining years.
  for (j in 2:TH){
    # Simulate deaths.
    GDEAD[i,j]<-rbinom(1,GLIVE[i,j-1],1-TPXG(x+j-1,1,m,b))
    # Count survivors.
    GLIVE[i,j]<-GLIVE[i,j-1]-GDEAD[i,j]
  }
}


### Simulated LogNormal Returns

# Set base parameters.
EXR<-0.04; SDR<-0.03; TH<-30; N<-10000
# Define placeholders.
PORET<-matrix(nrow=N,ncol=TH)
STPRV<-matrix(nrow=N,ncol=TH)
# Simulate N paths of TH returns.
for (i in 1:N){
  PORET[i,]<-exp(rnorm(TH,EXR,SDR))-1
}

#We use the vasicek model
PORET <- rt

### Temporary Life Income Annuity
TLIA<-function(x,y,r,m,b){
  APV<-function(t){exp(-r*t)*TPXG(x,t,m,b)}
  sum(APV(1:(y-x)))
}


### The Natural Tontine: Stochastic Returns and Deaths

# Parameters are set.
x<-65 #retirement age
r<-0.04 #long term assumed rate of return (ARR) net (after fees)
m<-90 # modal value in years of the distribution
b<-10 # dispersion (in years) coefficient
TH<-30 # time horizon
N<-10000 # number of simulations
GL0<-1000 # initial lives
f0<-100 # initial investment


#GLIVE[i,j] the first column shows the number of survivors at the end of year one for each simulation
#GDEAD[i,j] is just the deaths 
#PORET is the matrix of portfolio returns, rows are simulations, columns are years in the time horizon
#STPRV is the matrix of stochastic present values, columns are years in the time horizon

# Placeholders are defined.
DETFV<-matrix(nrow=N,ncol=TH) #modern tontine fund value (it is f0*G0 at the time 0)
TONDV<-matrix(nrow=N,ncol=TH) #tontine dividend payouts

# Vector of kappa values. kappa_i is the tontine dividend rate at period i
kappa<-c()
for (i in 1:TH){kappa[i]<-1/TLIA(x+i-1,x+TH,r,m,b)}
for (i in 1:N){
  # Dividend and fund value at end of year 1.
  TONDV[i,1]<-kappa[1]*f0
  DETFV[i,1]<-f0*GL0*(1+PORET[i,1]) - TONDV[i,1] *GLIVE[i,1]
  for (j in 2:TH){
    # Dividend and fund value at end of year j.
    TONDV[i,j]<-kappa[j]*DETFV[i,j-1]/GLIVE[i,j-1]
    DETFV[i,j]<-max(DETFV[i,j-1]*(1+PORET[i,j]) -TONDV[i,j]*GLIVE[i,j],0)
  }
}

plot(c(0,30),c(0,100000),type="n",
     xlab="YEARS after age 65",
     ylab="Fund Value")
title(main="MoTo: Natural Payout Vasizec model for returns",
      sub="(Original Pool Size = 1,000 )")
mtext(side=3, line=0.3,
      "Range: 99th (Highest, Green) & 1st (Lowest, Red)percentile"
      ,cex=1.1,font=3)
grid(ny=18,lty=20)
for (i in 1:30){
  pct99<-as.numeric(quantile(DETFV[,i],0.99))
  pct01<-as.numeric(quantile(DETFV[,i],0.01))
  points(i,pct99,col="green",pch=6)
  points(i,pct01,col="red",pch=2)
  segments(i,pct01,i,pct99,lwd=2)
  
}
