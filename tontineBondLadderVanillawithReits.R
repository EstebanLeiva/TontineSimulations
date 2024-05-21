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

calculate_dt <- function(periods, lifetable, n0){ #Tpx
  oMq <- rep(0, periods)
  oMq[1] <- 1 - lifetable[lifetable$Age == 65, "qx"] # 1-q
  for (j in 2:periods){
    oMq[j] <- oMq[j-1]*(1 - lifetable[lifetable$Age == 65+j, "qx"])
  }
  return((1-n0)*oMq)
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

REITS_return <- function(periods, n0, one_plus_rG){
  #initialize the return vector
  return <- rep(0, periods)
  return[1] <- n0*one_plus_rG[1]
  for (j in 2:periods){
    #each entry is the previous entry times 1+r
    return[j] <- return[j-1]*one_plus_rG[j]
  }
  return(return)
}

bondLadder <- function(N, periods, lifetable, number_individuals, n0){
  for (i in 1:N){
    r <- expected_longevity_return(periods, lifetable)
    G <- group_gain(periods, lifetable, r, number_individuals)
    one_plus_rG <- r*G + 1
    dt <- calculate_dt(periods, lifetable, n0)
    total_payout <- calculate_total_payout(periods, lifetable, one_plus_rG, dt)
    if (i == 1){
      bond_payout_matrix <- total_payout
    } else {
      bond_payout_matrix <- cbind(bond_payout_matrix, total_payout)
    }
  }
  return(bond_payout_matrix)
}

bondLadderREITS <- function(N, periods, lifetable, number_individuals, n0){
  for (i in 1:N){
    r <- expected_longevity_return(periods, lifetable)
    G <- group_gain(periods, lifetable, r, number_individuals)
    one_plus_rG <- r*G + 1
    dt <- calculate_dt(periods, lifetable, n0)
    total_payout <- REITS_return(periods, n0, one_plus_rG)
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

real_return_REITS <- 0.03
cost_1dollar_perpetuity <- 1/real_return_REITS
one_plus_r <- expected_longevity_return(periods, lifetable_2021) + 1
anual_expected_longevity_return <- prod(one_plus_r)^(1/30)-1
n0 <- (1/(1+anual_expected_longevity_return))^(30)


matrixBond <- bondLadder(N, periods, lifetable_2021, number_individuals, n0)

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
  pct99<-as.numeric(quantile(matrixBond[i,],1))
  pct01<-as.numeric(quantile(matrixBond[i,],0))
  points(i,pct99,col="green",pch=6)
  points(i,pct01,col="red",pch=2)
}
abline(h=1-n0,lty=2)

#multiplicar todos los tpx*proporcion inicial (1-n0)

matrixREITS <- bondLadderREITS(N, periods, lifetable_2021, number_individuals, n0)

plot(c(0,30),c(0.0,1.5),type="n",
     xlab="YEARS after age 65",
     ylab="Return of REITS")
title(main="Quantiles of Bond Ladder payouts",
      sub="(Original Pool Size = 10,000 )")
mtext(side=3, line=0.3,
      "Range: 99th (Highest, Green) & 1st (Lowest, Red)percentile"
      ,cex=1.1,font=3)
grid(ny=18,lty=20)
for (i in 1:30){
  pct99<-as.numeric(quantile(matrixREITS[i,],1))
  pct50 <- as.numeric(quantile(matrixREITS[i,],0.5))
  pct01<-as.numeric(quantile(matrixREITS[i,],0))
  points(i,pct99,col="green",pch=6)
  points(i,pct50,col="blue",pch=4)
  points(i,pct01,col="red",pch=2)
}
abline(h=0,lty=2)
abline(h=1,lty=2)

#the last row of the matrix put to 0
matrixBond[30,] <- 0

matrix <- matrixBond + matrixREITS
plot(c(0,30),c(0.5,2.5),type="n",
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
  pct50 <- as.numeric(quantile(matrix[i,],0.5))
  pct01<-as.numeric(quantile(matrix[i,],0))
  points(i,pct99,col="green",pch=6)
  points(i,pct50,col="blue",pch=4)
  points(i,pct01,col="red",pch=2)
}
abline(h=0,lty=2)
abline(h=1,lty=2)


###### GRAPH FOR PRESENTATION #####

Tpx <- c()
for (i in 1:40){
  value <- lifetable_2021[lifetable_2021$Age == 64+i, "Tpx"]
  Tpx <- c(Tpx, value)
}


plot(c(0,40),c(0,1.1),type="n",
     xlab="Years after age 65 (retirement age)",
     ylab="Survival probability")
title(main="Expected payments")
for (i in 1:40){
  if (i <= 18){
    points(i,1,col="red",pch=4)
  }
  points(i,Tpx[i],col="blue",pch=4)
}
abline(h=1,lty=2)
# vertical line
abline(v=18,lty=2)

