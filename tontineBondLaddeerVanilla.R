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

calculate_dt <- function(periods, lifetable){
  oMq <- rep(0, periods)
  oMq[1] <- 1 - lifetable[lifetable$Age == 65, "qx"] # 1-q
  for (j in 2:periods){
    oMq[j] <- oMq[j-1]*(1 - lifetable[lifetable$Age == 65+j, "qx"])
  }
  return(oMq)
}

calculate_total_payout <- function(periods, lifetable, one_plus_rG, dt){
  total_payout <- rep(0, periods)
  
  for (t in 1:periods){
    dummy <- dt[t]
    for (k in 1:t){
      dummy <- dummy*one_plus_rG[k]
    }
    total_payout[t] <- dummy
  }
  return(total_payout)
}

bondLadder <- function(N, periods, lifetable, number_individuals){
  for (i in 1:N){
    r <- expected_longevity_return(periods, lifetable)
    G <- group_gain(periods, lifetable, r, number_individuals)
    one_plus_rG <- r*G + 1
    dt <- calculate_dt(periods, lifetable)
    total_payout <- calculate_total_payout(periods, lifetable, one_plus_rG, dt)
    if (i == 1){
      bond_payout_matrix <- total_payout
    } else {
      bond_payout_matrix <- cbind(bond_payout_matrix, total_payout)
    }
  }
  return(bond_payout_matrix)
}

N <- 100
number_individuals <- 10000
periods <- 30

matrix <- bondLadder(N, periods, lifetable_2021, number_individuals)

plot(c(0,30),c(0.5,1.5),type="n",
     xlab="YEARS after age 65",
     ylab="Return of REITS")
title(main="Quantiles of Bond Ladder payouts",
      sub="(Original Pool Size = 10,000 )")
mtext(side=3, line=0.3,
      "Range: 99th (Highest, Green) & 1st (Lowest, Red)percentile"
      ,cex=1.1,font=3)
grid(ny=18,lty=20)
for (i in 1:30){
  pct99<-as.numeric(quantile(matrix[i,],1))
  pct01<-as.numeric(quantile(matrix[i,],0))
  points(i,pct99,col="green",pch=6)
  points(i,pct01,col="red",pch=2)
}
abline(h=1,lty=2)
abline(h=0,lty=2)

#multiplicar todos los tpx*proporcion inicial (1-n0)
