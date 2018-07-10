NSrates <- function(t, b0, b1, b2, tau1, spot=T, rates.in.percent=F)
{
  # Computation of instantaneous forward and spot rates within the Nelson-Siegel model
  #
  # t: vector of non-negative numbers giving the times to maturity (in years)
  # b0, b1, b2, tau1: model parameters
  # spot: Determines whether spot rates (spot=T) or instantaneous forward rates (spot=F)
  # should be calculated
  # rates.in.percent: If true, the calculated rates are returned in percentage quotation
  # and not in absolute values
  #
  # Components 0 and Inf of t are treated separately (see below)
  t.ok <- !(t==0 | is.infinite(t))
  rates <- numeric(length(t))
  if(spot)
  {
    rates[t.ok] <- b0+((b1+b2)*(1-exp(-t[t.ok]/tau1))*tau1)/t[t.ok]-b2*exp(-t[t.ok]/tau1)
    rates[t==0] <- b0+b1
    rates[is.infinite(t)] <- b0
  }
  else #forward, it uses this formula if spot=F in the original formula
  {
    rates[t.ok] <- b0+b1*exp(-t[t.ok]/tau1)+(b2*exp(-t[t.ok]/tau1)*t[t.ok])/tau1
    rates[t==0] <- b0+b1
    rates[is.infinite(t)] <- b0
  }
  if(rates.in.percent) 
    rates <- rates*100
  return(rates)
}
NSforward_result <- NSrates(seq(0,10,0.01),0.033,-0.01,0.025,2,spot=F)
NSspot_result <- NSrates(seq(0,10,0.01),0.033,-0.01,0.025,2)

plot(seq(0,10,0.01),NSforward_result,type="l",
       xlab="t",ylab="",col="blue",main="Nelson-Siegel forward and spot rates")
lines(seq(0,10,0.01),NSspot_result,col="red")
