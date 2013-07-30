truncate <- function(x, inf = -Inf, sup = Inf){
  if(missing(inf)) return(ifelse(x<sup,x,sup))
  if(missing(sup)) return(ifelse(x>inf,x,inf))
  x <- ifelse(x<sup,x,sup)
  x <- ifelse(x>inf,x,inf)
  return(x)
}

df_to_json <- function(df){
  jss <- adply(df, 1, toJSON)$V1
  jss <- sprintf("\t%s", jss)  
  jss <- paste(jss, c(rep(",", length(jss)-1), ""), sep="")
  jss <- c("[", jss, "]")
  jss
}