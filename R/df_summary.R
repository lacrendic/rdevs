df_summary <- function(dataframe){
  
  numcols <- laply(dataframe, is.numeric)
  
  d1 <-  ldply(seq(ncol(dataframe)), function(ncol){
    names(dataframe)[ncol]
    var <- dataframe[,names(dataframe)[ncol]]
    data.frame(variable = names(dataframe)[ncol],
               n_unique = length(unique(var)),
               class = class(var),
               na.prop = sum(is.na(var))/length(var))
    })
  
  d2 <- ldply(names(dataframe)[numcols], function(namevar){
    daux <- data.frame(t(as.vector(summary(dataframe[[namevar]]))))
    names(daux) <- names(summary(dataframe[[namevar]]))
    daux <- cbind(Variable = namevar, daux)
    })
  
  d3 <- ldply(names(dataframe)[!numcols], function(namevar){
    cbind(Variable=namevar, freqtable(dataframe[[namevar]]))
    })
  
  list(summary_classes = d1, summary_numeric_vars = d2, summary_nonnumeric_vars = d3)
}

summary_numvector <- function(v){
  data.frame(length = length(v),
             nas = sum(is.na(v)),
             min = min(v, na.rm = TRUE), 
             q1 = quantile(v, 0.25, na.rm = TRUE),
             median = quantile(v, 0.5, na.rm = TRUE),
             mean = mean(v, na.rm = TRUE),
             q3 = quantile(v, 0.75, na.rm = TRUE),
             max = max(v, na.rm = TRUE), 
             row.names=NULL)
}