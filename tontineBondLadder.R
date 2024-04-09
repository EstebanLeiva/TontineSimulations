#load lifetable data 
lifetable_2021 <- read.csv("data/LifeTablesbyYear/_2021 .csv")

#load TIPS interpolated rates
tips_rates <- read.csv("data/tipsRatesInterpolated.csv")

#create bond ladder matrix.
#income_PP: income per period
tips_ladder_OUTside_tontine <- function(income_PP, tips_rates){
  #initialize matrix: we assume the tips_rates csv has the number of periods we want
  bond_ladder <- matrix(0, nrow = nrow(tips_rates)+1, ncol = nrow(tips_rates))
  
  for (j in 1:ncol(bond_ladder)){
    for (i in 1:j){
      bond_ladder[i,j] <- income_PP / (1+tips_rates[tips_rates$Year == j, "Interpolated"]/100)^(j-i)
    }
  }
  return(bond_ladder)
}


tips_ladder_INside_tontine <- function(income_PP, lifetable, tips_rates, OUTladder){
  #initialize matrix the same size as OUTladder
  bond_ladder <- matrix(0, nrow = nrow(OUTladder), ncol = ncol(OUTladder))

  for (j in 1:ncol(bond_ladder)){
      bond_ladder[1,j] <- OUTladder[1,j] * (1 - lifetable[lifetable$Age == 65+j, "qx"])
  }
  #iterate over the rest upper triangular of the matrix with the exception of the first row
  for (j in 1:ncol(bond_ladder)){
      for (i in 2:nrow(bond_ladder)){
        if (i <= j-1){
          print(tips_rates[tips_rates$Year == j-i, "Interpolated"])
          print(i)
          print(j)
          print(bond_ladder[i-1,j])
          bond_ladder[i,j] <- bond_ladder[i-1,j] * (1 + tips_rates[tips_rates$Year == j-i, "Interpolated"]/100)        }
      }
    
  }
  return(bond_ladder)
}


#initialize the bond ladder
out <- tips_ladder_OUTside_tontine(1, tips_rates)
bond_ladder <- tips_ladder_INside_tontine(1, lifetable_2021, tips_rates, out)

#function that calculates the expected longevity return
longevity_return <- function(periods, lifetable){
  #initialize the return vector
  return <- rep(0, periods)

  for (j in 1:periods){
    return[j] <- lifetable[lifetable$Age == 65+j, "qx"]
  }
  
  return(return)
}

#initialize longevity return and add one to each entry
periods <- 30
one_plus_r <- longevity_return(periods, lifetable_2021) + 1

anual_rent_REITS_share <- 20
price_REITS_share <- 200
real_return_REITS <- 0.03
cost_1dollar_perpetuity <- 1/real_return_REITS
anual_expected_longevity_return <- prod(one_plus_r)^(1/30)-1
n0 <- (1/(1+anual_expected_longevity_return))^(30)
a0 <- cost_1dollar_perpetuity*n0
b0 <- 1 - n0

#function that calculates the value REITS and returns a vector with n periods
REITS_value <- function(periods, IN_bond_ladder, b0){
  #initialize the return vector
  return <- rep(0, periods)
  return[1] <- sum(IN_bond_ladder[1,])*b0
  for (j in 2:periods){
    #each entry is the previous entry times b0
    return[j] <- return[j-1]*(1+b0)
  }
  return(return)
}

#REITS_value <- REITS_value(periods, bond_ladder, b0)

number_shares_RA <- function(periods, n0, ones_plus_r){
  #initialize the return vector
  return <- rep(0, periods)
  return[1] <- n0*one_plus_r[1]
  for (j in 2:periods){
    #each entry is the previous entry times a0
    return[j] <- return[j-1]*one_plus_r[j]
    print(return[j])
  }
  return(return)
}

number_shares <- number_shares_RA(periods, n0, one_plus_r)

expected_payout_bond_ladder <- rep(b0,periods-1)
# append a 0 to expected payout bond ladder
expected_payout_bond_ladder <- c(expected_payout_bond_ladder,0)
expected_payout_REITS <- number_shares

#library(ggplot2)
#library(reshape2)

data <- data.frame(expected_payout_bond_ladder,expected_payout_REITS)
# first line should have the id category and the others the period number
data$id <- 1:nrow(data)
melted_data <- melt(data, id.vars = "id")
melted_data$variable <- factor(melted_data$variable, levels = c("expected_payout_REITS","expected_payout_bond_ladder"))
ggplot(melted_data, aes(x = id, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Plot",
       x = "Category",
       y = "Value",
       fill = "Variable") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black")


#### SIMULATIONS DEATHS ####
longevity_return <- function(periods, lifetable){
  #initialize the return vector
  return <- rep(0, periods)
  
  for (j in 1:periods){
    return[j] <- lifetable[lifetable$Age == 65+j, "qx"]
  }
  
  return(return)
}

#Simulation initialization
longevity_return_simulated <- function(periods, lifetable, number_individuals){
  #initialize the return vector
  return <- rep(0, periods)
  for (j in 1:periods){
    #return the fraction of deaths in the population
    deaths <- rbinom(1, number_individuals, lifetable[lifetable$Age == 65+j, "qx"])
    r <- deaths/number_individuals
    number_individuals <- number_individuals - deaths
    return[j] <- r/(1-r)
  }
  return(return)
}


N <- 1000 #number of simulations
number_individuals <- 100000
periods <- 30

anual_rent_REITS_share <- 20
price_REITS_share <- 200
real_return_REITS <- 0.03
cost_1dollar_perpetuity <- 1/real_return_REITS
one_plus_r_init <- longevity_return(periods, lifetable_2021) + 1
anual_expected_longevity_return <- prod(one_plus_r_init)^(1/30)-1
n0 <- (1/(1+anual_expected_longevity_return))^(30)
a0 <- cost_1dollar_perpetuity*n0
b0 <- 1 - n0

number_shares_RA_simulated <- function(periods, n0, number_individuals){
  #initialize the return vector
  return <- rep(0, periods)
  one_plus_r <- longevity_return_simulated(periods, lifetable_2021, number_individuals) + 1
  return[1] <- n0*one_plus_r[1]
  for (j in 2:periods){
    #each entry is the previous entry times a0
    return[j] <- return[j-1]*one_plus_r[j]
  }
  return(return)
}

for (i in 1:N){
  number_shares <- number_shares_RA_simulated(periods, n0, number_individuals)
  if (i == 1){
    number_shares_matrix <- number_shares
  } else {
    number_shares_matrix <- cbind(number_shares_matrix, number_shares)
  }
}

plot(c(0,30),c(0,2.1),type="n",
     xlab="YEARS after age 65",
     ylab="Return of REITS")
title(main="Quantiles of REITS return of Bond Ladder",
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
quantile(number_shares_matrix[30,],0.01)


### Full REITS
expected_payout_FULL_REITS <- number_shares_RA(periods,1,ones_plus_r)
#do a bar plot of the expected payout of the full REITS
data <- data.frame(expected_payout_FULL_REITS)
# first line should have the id category and the others the period number
data$id <- 1:nrow(data)
#use ggplot to plot the barplot
ggplot(data, aes(x = id, y = expected_payout_FULL_REITS)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Plot",
       x = "Category",
       y = "Value",
       fill = "Variable")

#join the two plots before in one
data <- data.frame(expected_payout_bond_ladder,expected_payout_REITS,expected_payout_FULL_REITS)
# first line should have the id category and the others the period number
data$id <- 1:nrow(data)
melted_data <- melt(data, id.vars = "id")
melted_data$variable <- factor(melted_data$variable, levels = c("expected_payout_REITS","expected_payout_bond_ladder","expected_payout_FULL_REITS"))
ggplot(melted_data, aes(x = id, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Plot",
       x = "Category",
       y = "Value",
       fill = "Variable")


#### SIMULATIONS RETURNS ####
#### THIS IS THE VERSION THAT WORKS ####
lifetable_2021 <- read.csv("data/LifeTablesbyYear/2021.csv")

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

REITS_returns_std <- 0.1
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

plot(c(0,30),c(0,1.5),type="n",
     xlab="YEARS after age 65",
     ylab="Return of REITS")
title(main="Quantiles of REITS return of Bond Ladder",
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

#Tpx (L0) vs 1-REITS bond

plot(c(0,30),c(-0.2,1.5),type="n",
     xlab="YEARS after age 65",
     ylab="Return of REITS")
title(main="Quantiles of REITS return of Bond Ladder",
      sub="(Original Pool Size = 10,000 )")
mtext(side=3, line=0.3,
      "Range: 99th (Highest, Green) & 1st (Lowest, Red)percentile"
      ,cex=1.1,font=3)
grid(ny=18,lty=20)
for (i in 1:30){
  
  pct99<-as.numeric(quantile(1-number_shares_matrix[i,],1))
  pct01<-as.numeric(quantile(1-number_shares_matrix[i,],0))
  Tpx <- lifetable_2021[lifetable_2021$Age == 64+i, "Tpx"]
  px <- lifetable_2021[lifetable_2021$Age == 64+i, "px"]
  points(i,pct99,col="green",pch=6)
  points(i,pct01,col="red",pch=2)
  points(i,Tpx,col="blue",pch=4)
  points(i,px,col="black",pch=3)
  
  
}
abline(h=0,lty=2)
#by this graph it does not appear that L0 and the bond ladder are the same.

