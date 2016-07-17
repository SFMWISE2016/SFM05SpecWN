rm(list = ls(all = TRUE))
graphics.off()

# The function to calculate the theoretical option price 
bscall = function(S, sig, maturity, K, r,t0){
  #maturity = mat
  #S=S0
  tau = maturity - t0 
  d2 = (log(S/K) + (r - sig^2/2) * tau)/(sig * sqrt(tau))
  d1 = d2 + sig * sqrt(tau)
  call = S * pnorm(d1) - K * exp(-r * tau) * pnorm(d2) 
  return(call)
}

# Calculate the cost to hedging a call option
costcal = function(S0, sig, maturity, K, r, n, t0) {
  #maturity=mat
  dt   = (maturity - t0)/n             # period between steps n 
  t   = seq(t0, maturity, l = n)      # maturity - t0 divided in n intervals
  tau = maturity - t                  # time to maturity
  
  # Simulate the stock price path
  Wt 	= c(0, sqrt(dt) * cumsum(rnorm(n - 1, 0, 1)))
  S 	= S0 * exp((r - 0.5 * sig^2) * t + sig * Wt)
  
  # Compute delta and the associated hedging costs
  y               = (log(S/K) + (r - sig^2/2) * tau)/(sig * sqrt(tau))
  delta           = pnorm(y + sig * sqrt(tau))
  hedge.costs     = c(delta[1] * S[1], (delta[2:n] - delta[1:(n - 1)]) * S[2:n])
  cum.hedge.costs = cumsum(hedge.costs)
  
  # Result
  cost   = cum.hedge.costs[n]
  ST     = S[n]
  result = ifelse(ST>K, cost-K, cost)
  return(result)
}

# Declare stock price variables
N = c(4,5,10,20,40,80) # periods (steps)
S0  = 98        # initial stock price
sig = 0.2       # volatility (uniform distributed on 0.1 to 0.5)

# Declare option pricing variables
r   = 0.05      # interest rate (uniform distributed on 0 to 0.1)
K   = 100       # exerise price
t0  = 6/52      # current time (1 week = 1/52)
mat = 26/52     # maturity

performance = sapply(1:length(N), function(j){
  n=N[j]
  
  costsim=sapply(1:100000,function(i){
    cost = costcal(S0 = S0, sig = sig, maturity = mat, K = K, r = r, n = n, t0 = t0)
    return(cost)
  })
  
  call = bscall (S = S0, sig = sig, maturity = mat, K = K, r = r, t0 = t0)
  
  L = sd(costsim) / call
  
  return(L)
})









