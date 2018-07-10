interpolate.zero.rates <- function(t,maturities,zero.rates){
  # Linearly interpolate between given zero rates to find an
  # approximate zero rate for a certain maturity t in between
  #
  # t: vector of non-negative numbers giving the maturity(ies) for which
  # zero rates should be interpolated
  # maturities: maturities (in years) corresponding to the given zero.rates
  # These must be sorted in ascending order!
    # zero.rates: zero rates corresponding to the given maturities
    if(length(maturities) != length(zero.rates))
      return("Vectors maturities and zero.rates must have the same lengths!")
  lm <- length(maturities)
  rates <- numeric(length(t))
  for(i in 1:length(t)){
    n <- sum(maturities < t[i])
    # t[i] is smaller than maturities[1] or greater than maturities[lm]
    if(n == 0 || n == lm) 
      rates[i] <- (n==0)*zero.rates[1]+(n==lm)*zero.rates[lm]
    else{
      # We have maturities[n] < t[i] <= maturities[n+1]
      T1 <- maturities[n]
      T2 <- maturities[n+1]
      r1 <- zero.rates[n]
      r2 <- zero.rates[n+1]
      rates[i] <- ((T2-t[i])/(T2-T1))*r1+((t[i]-T1)/(T2-T1))*r2
    }
  }
  return(rates)
}
interpolate.zero.rates(c(0.3,0.7,1.25,2.75,3.5),c(0.5,1,2,3),c(0.015,0.017,
                          0.021,0.025))
