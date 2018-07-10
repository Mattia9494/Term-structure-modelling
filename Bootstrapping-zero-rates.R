bootstrap.zero.rates <- function(nominals,maturities,coupons,prices){
  # This function calculates the risk-free zero rates implied by the prices
  # of coupon bearing bonds using the bootstrap method
  # The bonds are assumed to be ordered by ascending maturities!
  l <- length(prices)
  if(length(nominals) != l || length(coupons) != l || length(maturities) != l)
    return("All arguments must have the same length!")
  # || means "or"
  
  # Generate an empty vector of length l in which the zero rates will be written
  zero.rates <- numeric(l)
  # loop over all maturities
  for(i in 1:l){
    # risk-free rates implied by the bond with shortest maturity
    # and all zero-coupon bonds with larger maturities
    if(i == 1 || coupons[i] == 0.0)
    {
      cash <- nominals[i]*(1+coupons[i]*maturities[i])
      zero.rates[i] <- -1/maturities[i]*log(prices[i]/cash)
    }
    else{
      # Determine payment dates of the coupon bearing bonds
      # payment frequency is assumed to be the difference between
      # the last two meturities
      freq <- maturities[i]-maturities[i-1]
      dates <- rev(seq(maturities[i],0,-freq))
      # Exclude zero (coupon payments at the present time t=0 are assumed
      # to have already been made)
      dates <- dates[dates > 0]
      # Extract the already known zero rates corresponding to all payment dates
      # except the last one from the vector zero.rates
      if(maturities[i] %% freq == 0)
          rates <- zero.rates[1:(i-1)][maturities[1:(i-1)] %% freq == 0]
      else
        rates <- zero.rates[1:(i-1)][(maturities[1:(i-1)]-maturities[1]) %% freq == 0]
      # Calculate the present value of all coupon payments up to the last but
      # one payment date
      cash <- sum(rep(nominals[i]*coupons[i]*freq,length(dates)-1)
                  *exp(-rates*dates[-length(dates)]))
      # Determine the zero rate corresponding to maturities[i]
      zero.rates[i] <- -1/dates[length(dates)]
      *log((prices[i]-cash)/((1+coupons[i]*freq)*nominals[i]))
    }
  }
  return(zero.rates)
}

#now we apply it
bootstrap.zero.rates(rep(100,4),c(0.5,1,2,3),c(0,0,0.03,0.04),c(99.25,98.31,101.71,104.25))
