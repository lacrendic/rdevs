df_summary <- function(dataframe){
  
  numcols <- laply(d, is.numeric)
  
  d1 <- ldply(names(dataframe)[numcols], function(namevar){
    daux <- data.frame(t(as.vector(summary(dataframe[[namevar]]))))
    names(daux) <- names(summary(dataframe[[namevar]]))
    daux <- cbind(Variable = namevar, daux)
  })
  
  d2 <- ldply(names(dataframe)[!numcols], function(namevar){
    cbind(Variable=namevar, freqtable(dataframe[[namevar]]))
  })
  
  list(summary_numeric_vars = d1, summary_nonnumeric_vars = d2)
}
