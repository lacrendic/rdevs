truncate <- function(x, inf = -Inf, sup = Inf){
  
  if(missing(inf)) return(ifelse(x<sup,x,sup))
  
  if(missing(sup)) return(ifelse(x>inf,x,inf))
  
  x <- ifelse(x<sup,x,sup)
  
  x <- ifelse(x>inf,x,inf)
  
  return(x)
}

table_bivariate <- function(variable, indicator){
  library(dplyr)
  library(scales)
  n <- length(variable)
  t <- data.frame(variable, indicator) %.%
    group_by(variable) %.%
    summarize(freq = n(), percent = freq/n, indicator.mean = mean(indicator)) %.%
    mutate(freq.pretty = prettyNum(freq, big.mark="."),
           indicator.mean.pretty = percent(indicator.mean))
  t
}
