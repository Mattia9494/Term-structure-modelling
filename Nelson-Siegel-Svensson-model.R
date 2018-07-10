SVrates <- function(t, b0, b1, b2, b3, tau1, tau2, spot=T, rates.in.percent=F){
  # Computation of instantaneous forward dnd spot rates within the Svensson model
  #
  # t: vector of non-negative numbers giving the times to maturity (in years)
  # b0, b1, b2, b3, tau1, tau2: model parameters
  # spot: Determines whether spot rates (spot=T) or instantaneous forward rates (spot=F)
  # should be calculated
  # rates.in.percent: If true, the calculated rates are returned in percentage quotation
  # and not in absolute values
  #
  # Components 0 and Inf of t are treated separately (see below)
  t.ok <- !(t==0 | is.infinite(t))
  rates <- numeric(length(t))
  if(spot){
    rates[t.ok] <- b0+((b1+b2)*(1-exp(-t[t.ok]/tau1))*tau1)/t[t.ok]-b2*exp(-t[t.ok]/tau1)+
      (b3*(1-exp(-t[t.ok]/tau2))*tau2)/t[t.ok]-b3*exp(-t[t.ok]/tau2)
    rates[t==0] <- b0+b1
    rates[is.infinite(t)] <- b0
  }
  else{
    rates[t.ok] <- b0+b1*exp(-t[t.ok]/tau1)+(b2*exp(-t[t.ok]/tau1)*t[t.ok])/tau1+
      (b3*exp(-t[t.ok]/tau2)*t[t.ok])/tau2
    rates[t==0] <- b0+b1
    rates[is.infinite(t)] <- b0
  }
  if(rates.in.percent) rates <- rates*100
  return(rates)
}
SVforward_result <- SVrates(seq(0,10,0.01),0.077,-0.01,0.025,0.044,2,0.5,spot=F)
SVspot_result <- SVrates(seq(0,10,0.01),0.077,-0.01,0.025,0.044,2,0.5)


plot(seq(0,10,0.01),SVforward_result,type="l",xlab="t",ylab="",
     col="blue",main="Svensson forward and spot rates")
lines(seq(0,10,0.01),SVspot_result,col="red")
