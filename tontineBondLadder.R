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

REITS_value <- REITS_value(periods, bond_ladder, b0)

number_shares_RA <- function(periods, n0, ones_plus_r){
  #initialize the return vector
  return <- rep(0, periods)
  return[1] <- n0
  for (j in 2:periods){
    #each entry is the previous entry times a0
    return[j] <- return[j-1]*one_plus_r[j]
  }
  return(return)
}

number_shares <- number_shares_RA(periods, n0, one_plus_r)

expected_payout_bond_ladder <- rep(b0,periods)

expected_payout_REITS <- number_shares

library(ggplot2)
library(reshape2)

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
       fill = "Variable")

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

