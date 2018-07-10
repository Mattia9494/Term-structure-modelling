#I want to use the optim() function to fit the NS and SV models to observed market data.
###IMPORTANT: TO MAKE IT WORK YOU HAVE TO MAKE THE NS/SV FUNCTION RUN BEFORE

NSSrates.est <- function(dates,rates,model="NS",spot=T,p0=c(1,1,1,1),p1=c(0,1))
{
  # Estimating the parameters of the Nelson-Siegel and Svensson models
  # via least squares optimization
  #
  # dates: vector of non-negative numbers giving the times to maturity (in years)
  # rates: zero rates corresponding to the given maturity dates
  #
  # These must be given in absolute numbers, e.g., 3% = 0.03
  # model: specifies whether the parameters of the Nelson-Siegel model (model="NS")
  #
  # or of the Svensson model (model="SV") should be estimated
  # spot: specifies whether given rates are spot (spot=T) or forward rates (spot=F)
  # p0: starting values (b0,b1,b2,tau1) for the optimization in the Nelson-Siegel model
  # p1: Additional starting values (for beta3 and tau2) for the estimation in the
  #
  # Svensson model
  if(length(dates) != length(rates))
    return("Vectors dates and rates must have the same lengths!")
  if(any(dates < 0)) 
    return("Vectors dates must have non-negative elements!")
  # Function to be minimized by optim()
  # The sum of the squared differences between model and market prices should be as
  # small as possible
  NSSrates.min <- function(par,dates,rates,model,spot)
  {
    if(model=="NS") 
      return(sum((NSrates(dates,par[1],par[2],par[3],par[4],spot=spot,T)-100*rates)^2))
    else 
      return(sum((SVrates(dates,par[1],par[2],par[3],par[4],par[5],par[6],spot=spot,T)-100*rates)^2))
  }
  # First we always estimate the parameters of the Nelson-Siegel model
  result <- optim(p0,NSSrates.min,dates=dates,rates=rates,model="NS",spot=spot)$par
    if(model=="SV")
    {
    # For the parameter estimation in the Svensson model, we take the already
    # estimated Nelson-Siegel parameters as starting values for b0, b1, b2, and tau1.
    # The starting values for the additional parameters beta3 and tau2 are given by
    # the vector p1.
    start <- c(result[1:3],p1[1],result[4],p1[2])
    result <- optim(start,NSSrates.min,dates=dates,rates=rates,model="SV",spot=spot)$par
    }
  return(result)
}
par.ns <- NSSrates.est(c(0.5,1,2,3),c(0.015,0.017,0.021,0.025))
par.ns
#Let's try to make a graphical representation to check the fit
plot(seq(0,3,0.01),NSrates(seq(0,3,0.01),par.ns[1],par.ns[2],par.ns[3],par.ns[4]),
     type="l",xlab="t",ylab="",col="blue",main="Real and estimated Nelson-Siegel rates")
points(c(0.5,1,2,3),c(0.015,0.017,0.021,0.025),col="red",pch=20)

#it works let's try with the other method Svensson
par.sv <- NSSrates.est(c(0.5,1,2,3),c(0.015,0.017,0.021,0.025),model="SV")
par.sv
plot(seq(0,3,0.01),SVrates(seq(0,3,0.01),par.sv[1],par.sv[2],par.sv[3],par.sv[4],
                           par.sv[5],par.sv[6]),type="l",xlab="t",ylab="",col="blue",
                          main="Real and estimated Svensson rates")
points(c(0.5,1,2,3),c(0.015,0.017,0.021,0.025),col="red",pch=20)


