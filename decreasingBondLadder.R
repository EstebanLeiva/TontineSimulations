set.seed(1234)

lifetable_2021 <- read.csv("data/LifeTablesbyYear/2021.csv")

longevity_return <- function(periods, lifetable){
  #initialize the return vector
  return <- rep(0, periods)
  
  for (j in 1:periods){
    return[j] <- lifetable[lifetable$Age == 65+j, "qx"]
  }
  
  return(return)
}

periods <- 30

anual_rent_REITS_share <- 20
price_REITS_share <- 200
real_return_REITS <- 0.03
cost_1dollar_perpetuity <- 1/real_return_REITS
one_plus_r <- longevity_return(periods, lifetable_2021) + 1
anual_expected_longevity_return <- prod(one_plus_r)^(1/30)-1
n0 <- (1/(1+anual_expected_longevity_return))^(30)
a0 <- cost_1dollar_perpetuity*n0
b0 <- 1 - n0

number_shares_RA <- function(periods, n0, one_plus_r){
  #initialize the return vector
  return <- rep(0, periods)
  return[1] <- n0*one_plus_r[1]
  for (j in 2:periods){
    #each entry is the previous entry times a0
    return[j] <- return[j-1]*one_plus_r[j]
  }
  return(return)
}

Tpx <- c()
for (i in 1:30){
  value <- lifetable_2021[lifetable_2021$Age == 64+i, "Tpx"]
  Tpx <- c(Tpx, value)
  
}

number_shares <- number_shares_RA(periods, n0, one_plus_r)
#plot number_shares
#y axis limits 
plot(number_shares, type = "l", xlab = "Periods", ylab = "Number of shares", main = "Number of shares in REITS")
#plot 1 - number_shares
plot(1-number_shares, type = "l", xlab = "Periods", ylab = "Expected payout", main = "Decreasing bond ladder")
# in the same figure plot tpx
lines(Tpx, col = "red")
# add legend
legend("topright", legend = c("Bond", "Tpx"), col = c("black", "red"), lty = 1:2, cex = 0.8)

##### Present Value #####

tips_rates <- read.csv("data/tipsRatesInterpolated.csv")
tips_rates <- tips_rates[,2]
tips_rates <- rep(2, periods) #in percentage

discount_rates <- rep(0, periods)

for (j in 1:periods){
  if (j == 1){
    discount_rates[j] <- 1+(tips_rates[j]/100)
  }
  else{
    discount_rates[j] <- (1+(tips_rates[j]/100))*discount_rates[j-1]
  }
}

#get present value using discount rates
present_value_decreasing <- sum((1-number_shares)*(1/discount_rates))
present_value_fixed <- sum(b0*(1/discount_rates))
present_value_tpx <- sum(Tpx*(1/discount_rates))

# monte carlo simulation with decreasing bond ladder
set.seed(1234)

longevity_return_simulated <- function(periods, lifetable, number_individuals){
  #initialize the return vector
  return <- rep(0, periods)
  for (j in 1:periods){
    #return the fraction of deaths in the population
    deaths <- rbinom(1, number_individuals, lifetable[lifetable$Age == 64+j, "qx"])
    r <- deaths/number_individuals
    number_individuals <- number_individuals - deaths
    return[j] <- r/(1-r)
  }
  return(return)
}

simulate_returns <- function(periods, mean, sd){
  return(rlnorm(periods, mean, sd))
}

longevity_return <- function(periods, lifetable){
  #initialize the return vector
  return <- rep(0, periods)
  
  for (j in 1:periods){
    r <- lifetable[lifetable$Age == 64+j, "qx"]
    return[j] <- r/(1-r)
  }
  
  return(return)
}


N <- 1000 #number of simulations
number_individuals <- 10000
periods <- 30

anual_rent_REITS_share <- 20
price_REITS_share <- 200
real_return_REITS <- 0.03
cost_1dollar_perpetuity <- 1/real_return_REITS
one_plus_r_init <- longevity_return(periods, lifetable_2021) + 1
anual_expected_longevity_return <- prod(one_plus_r_init)^(1/30)-1
n0 <- (1/(1+anual_expected_longevity_return))^(periods)
a0 <- cost_1dollar_perpetuity*n0
b0 <- 1 - n0

number_shares_RA <- function(periods, n0, one_plus_r){
  #initialize the return vector
  return <- rep(0, periods)
  return[1] <- n0*one_plus_r[1]
  for (j in 2:periods){
    #each entry is the previous entry times 1+r
    return[j] <- return[j-1]*one_plus_r[j]
  }
  return(return)
}

REITS_payout <- function(periods, number_shares, std){
  #initialize the return vector
  return <- rep(0, periods)
  simulation <- simulate_returns(periods, 0, std)
  #print(simulation[1])
  for (j in 1:periods){
    return[j] <- number_shares[j]*simulation[j]
  }
  return(return)
}

REITS_returns_std <- 0
for (i in 1:N){
  one_plus_r <- longevity_return_simulated(periods, lifetable_2021, number_individuals) + 1
  #one_plus_r <- one_plus_r_init
  number_shares <- number_shares_RA(periods, n0, one_plus_r)
  if (i == 1){
    number_shares_matrix <- REITS_payout(periods, number_shares, REITS_returns_std)
  } else {
    number_shares_matrix <- cbind(number_shares_matrix, REITS_payout(periods, number_shares, REITS_returns_std))
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
  
  pct99<-as.numeric(quantile(1-number_shares_matrix[i,],1))
  pct01<-as.numeric(quantile(1-number_shares_matrix[i,],0))
  points(i,pct99,col="green",pch=6)
  points(i,pct01,col="red",pch=2)
  
  
}
abline(h=1,lty=2)
abline(h=0,lty=2)

# que el reits sea la diferencia con el Tpx
one_plus_r <- longevity_return(periods, lifetable_2021) + 1

Tpx <- c()
for (i in 1:30){
  value <- lifetable_2021[lifetable_2021$Age == 64+i, "Tpx"]
  Tpx <- c(Tpx, value)
}
number_shares_Tpx <- function(Tpx, one_plus_r, periods){
  return <- rep(0, periods)
  return[1] <- (1-Tpx[1])*one_plus_r[1]
  for (j in 2:periods){
    #each entry is the previous entry times a0
    return[j] <- return[j-1]*one_plus_r[j]
  }
  return(return)
}

number_shares <- number_shares_Tpx(Tpx, one_plus_r, periods)

plot(c(0,30),c(-0.5,1.5),type="n",
     xlab="YEARS after age 65",
     ylab="Return of REITS")
title(main="REITS Payout")
grid(ny=18,lty=20)
for (i in 1:periods){
  points(i,number_shares[i],col="green",pch=1)
  points(i,Tpx[i],col="red",pch=1)
  points(i,Tpx[i] + number_shares[i],col="gray",pch=1)
}
abline(h=1,lty=2)
abline(h=0,lty=2)
# tpx igual a los pagos (no estoy en una tontina), con los mortality credits tengo un pago seguro de 1
