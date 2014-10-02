truncate <- function(x, inf = -Inf, sup = Inf){
  
  if(missing(inf)) return(ifelse(x<sup,x,sup))
  
  if(missing(sup)) return(ifelse(x>inf,x,inf))
  
  x <- ifelse(x<sup,x,sup)
  
  x <- ifelse(x>inf,x,inf)
  
  return(x)
}
