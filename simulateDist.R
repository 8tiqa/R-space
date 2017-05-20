
## Computes the two-sided empirical confidence intervals or quantiles for the input values
comp.ci <- function(vec, quantile = 0.05){
  ## Compute the upper and lower emperical quantiles
  lower <- quantile/2.0
  upper <- 1.0 - lower
  c(quantile(vec, probs = lower, na.rm = TRUE),
    quantile(vec, probs = upper, na.rm = TRUE))
}

## Plots a histogram and returns summary statistics
dist.summary <- function(dist, name, num.bins = 120){
  ## function to plot and print a summary
  ## of the distribution  
  maxm <- max(dist)
  minm <- min(dist)
  bw <- (maxm - minm)/num.bins
  breaks <- seq(minm - bw/2, maxm + bw/2, by = bw)
  hist(dist, col = 'blue', breaks = breaks, xlab = name,
       main = paste('Distribution of ', name))
  
  std <- round(sd(dist), digits = 2)
  print(paste('Summary of', name, '; with std = ', std))
  print(summary(dist))
} 

## Simulates a Normal Distribution
sim.normal <- function(num, mean = 600, sd = 30){
  ## Simulate from a Normal distribution
  dist = rnorm(num, mean, sd)
  titl = paste('Normal: ', as.character(num), ' values')
  dist.summary(dist, titl)     
  print('Empirical 95% CIs')
  print(comp.ci(dist))
  NULL
}

nums <- c(100, 1000, 10000, 100000)
lapply(nums, sim.normal)

## Simulates a Poisson Distribution
sim.poisson <- function(num, mean = 600){
  ## Simulate from a Poisson distribution
  dist = rpois(num, mean)
  titl = paste('Poisson: ', as.character(num), ' values')
  dist.summary(dist, titl)    
  print('Empirical 95% CIs')
  print(comp.ci(dist))
  NULL
}

sim.poisson(100000)

## LAB EXAMPLE: Lemonade Stand Income

## Generates the profit from the uniform distribution
profits <- function(num){  
  unif <- runif(num)
  ifelse(unif < 0.3, 5,
         ifelse(unif < 0.6, 3.5, 4))
}

prfts <- profits(100000)
dist.summary(prfts, 'profits')

tips <- function(num){
  ## Generates the tips from the uniform distribution
  unif <- runif(num)
  ifelse(unif < 0.5, 0,
         ifelse(unif < 0.7, 0.25, 
                ifelse(unif < 0.9, 1, 2)))
}

tps <- tips(100000)
dist.summary(tps, 'tips')


sim.lemonade <- function(num, mean = 600, sd = 30, pois = FALSE){
  ## Simulate the profits and tips for
  ## a lemonade stand.
  
  ## number of customer arrivals
  if(pois){
    arrivals <- rpois(num, mean)
  } else {
   arrivals <- rnorm(num, mean, sd) 
  }
  dist.summary(arrivals, 'customer arrivals per day')
  
  ## Compute distibution of average profit per arrival
  proft <- profits(num) 
  dist.summary(proft, 'profit per arrival')
  
  ## Total profits are profit per arrival 
  ## times number of arrivals.
  total.profit <- arrivals * proft 
  dist.summary(total.profit, 'total profit per day')
  
  ## Compute distribution of average tips per arrival
  tps <- tips(num) 
  dist.summary(tps, 'tips per arrival')
  
  ## Compute average tips per day
  total.tips <- arrivals * tps
  dist.summary(total.tips, 'total tips per day')
  
  ## Compute total profits plus total tips and normalize.
  total.take <- total.profit + total.tips
  dist.summary(total.take, 'total net per day')
}


## simulation for 100,000 values, using the default mean and standard deviation
sim.lemonade(100000)

## simulation for 100,000 values with the expected number of customer arrivals per day increased to 1200 
## and a standard deviation of 40
sim.lemonade(10000, 1200, 40)


