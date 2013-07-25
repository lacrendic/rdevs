generatesimdata <- function(n = 123456, p = .6, seed = 1, n.continous.var = 10, n.categoric.var = 10, max.levels.cat = 10){
  set.seed(seed) 
  
  data <- data.frame(good_bad = rbinom(n, size=1, prob=p))
  
  for(var in seq(n.continous.var)){
    data[[sprintf("var_continous_%s", var)]] <- rnorm(n, mean=data$good_bad, sd=runif(1))
  }
  
  
  for(var in seq(n.categoric.var)){
    
    ncats <- sample(2:max.levels.cat, prob=sample(2:max.levels.cat), size=1)
    
    
    
    
    
    data$good_bad
    
    var <- 
    data[[sprintf("var_categoric_%s", var)]] <- rnorm(n, mean=data$good_bad, sd=runif(1))
  }  
  
  return(data)
}
  
generatesimdata()