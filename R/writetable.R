writetable <- function(data, name = "data.txt",  row.names = F, ...){
  
  if(class(data)=="table"){
    
    data <- as.data.frame.matrix(data)
    
  }
  
  if(length(unlist(strsplit(name, "\\.")))==1){
  
    name <- paste(name, ".txt", sep = "")
    
  }
  
  ext <- tolower(unlist(strsplit(name, "\\."))[length(unlist(strsplit(name, "\\.")))])
  
  if(!ext %in% c("txt","csv","xlsx","xls", "psv", "json")){
	  
    stop("No posible extension")
    
  }
  
  if(ext == "txt"){
    
    write.table(data, name, dec = ".", sep = "\t", col.names = TRUE, quote = FALSE, row.names = row.names, ...)
    
  } else if(ext == "csv"){
    
    write.table(data, name, dec = ",", sep = ";", col.names = TRUE, quote = FALSE, row.names = row.names, ...)
    
  } else if(ext == "psv"){
    
    write.table(data, name, dec = ".", sep = "|", col.names = TRUE, quote = FALSE, row.names = row.names, ...)
    
  } else if(ext %in% c("xlsx","xls")){
    
    require(xlsx)
    wb <- createWorkbook()
    style1 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border()
    addDataFrame(data, createSheet(wb), colnamesStyle=style1, row.names=row.names)
    saveWorkbook(wb, name)
    
  } else if(ext == "json"){
    
    writeLines(text=df_to_json(data), con=name)
    
  }
  
}

df_to_json <- function(df){
  
  library(plyr)
  library(rjson)
  
  jss <- adply(df, 1, toJSON)$V1
  jss <- sprintf("\t%s", jss)  
  jss <- paste(jss, c(rep(",", length(jss)-1), ""), sep="")
  jss <- c("[", jss, "]")
  
  jss
}