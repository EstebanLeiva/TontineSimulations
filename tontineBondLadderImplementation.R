#Programar Tontine Bond Ladder con G Montecarlo
set.seed(1234)

lifetable_2021 <- read.csv("data/LifeTablesbyYear/2021.csv")

### q/(1-q) for each age
expected_longevity_return <- function(periods, lifetable){
  #initialize the return vector
  r <- rep(0, periods)
  
  for (j in 1:periods){
    r[j] <- lifetable[lifetable$Age == 65+j, "qx"]/(1-lifetable[lifetable$Age == 65+j, "qx"])
  }
  
  return(r)
}

### G for each period
group_gain <- function(periods, lifetable, r, n){
  #initialize the return vector
  G <- rep(0, periods)
  
  for (j in 1:periods){
    deaths <- rbinom(1, n, lifetable[lifetable$Age == 65+j, "qx"])
    n <- n - deaths
    G[j] <- deaths/(r[j]*n)
  }
  
  return(G)
}

### Number of shares of REITS for each period
number_shares_RA <- function(periods, n0, one_plus_rG){
  #initialize the return vector
  return <- rep(0, periods)
  return[1] <- n0*one_plus_rG[1]
  for (j in 2:periods){
    #each entry is the previous entry times 1+r
    return[j] <- return[j-1]*one_plus_rG[j]
  }
  return(return)
}

N <- 1000 #number of simulations
number_individuals <- 1000
periods <- 30
real_return_REITS <- 0.03
one_plus_r_init <- expected_longevity_return(periods, lifetable_2021) + 1
anual_expected_longevity_return <- prod(one_plus_r_init)^(1/30)-1
n0 <- (1/(1+anual_expected_longevity_return))^(periods)

for (i in 1:N){
  r <- expected_longevity_return(periods, lifetable_2021)
  G <- group_gain(periods, lifetable_2021, r, number_individuals)
  one_plus_rG <- r*G + 1
  number_shares <- number_shares_RA(periods, n0, one_plus_rG)
  if (i == 1){
    number_shares_matrix <- number_shares
  } else {
    number_shares_matrix <- cbind(number_shares_matrix, number_shares)
  }
}

plot(c(0,30),c(-0.5,1.5),type="n",
     xlab="YEARS after age 65",
     ylab="Return of REITS")
title(main="Quantiles of Bond Ladder payouts",
      sub="(Original Pool Size = 10,000 )")
mtext(side=3, line=0.3,
      "Range: 99th (Highest, Green) & 1st (Lowest, Red)percentile"
      ,cex=1.1,font=3)
grid(ny=18,lty=20)
for (i in 1:30){
  
  pct99<-as.numeric(quantile(number_shares_matrix[i,],1))
  pct01<-as.numeric(quantile(number_shares_matrix[i,],0))
  points(i,pct99,col="green",pch=6)
  points(i,pct01,col="red",pch=2)
  
  
}
abline(h=1,lty=2)
abline(h=0,lty=2)

