df_to_json <- function(df){
  
  library(plyr)
  library(rjson)
  
  jss <- adply(df, 1, toJSON)$V1
  jss <- sprintf("\t%s", jss)  
  jss <- paste(jss, c(rep(",", length(jss)-1), ""), sep="")
  jss <- c("[", jss, "]")
  
  jss
}