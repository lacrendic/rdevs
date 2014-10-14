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

