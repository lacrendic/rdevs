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
    
    suppressPackageStartupMessages(library(XLConnect))
    wb <- loadWorkbook(name, create = TRUE)
    createSheet(wb, name = 'sheet')
    writeWorksheet(wb, data, sheet = 'sheet')
    saveWorkbook(wb)
    
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

write.list.tables.xlsx <- function(list.tables, name){
  
  library(XLConnect)

  if(is.null(names(list.tables))){
    names(list.tables) <- paste0("Sheet", seq(length(list.tables)))
  }
  
  wb <- loadWorkbook(name, create = TRUE)
  
  for(table.name in names(list.tables)){
    createSheet(wb, name = table.name)
    writeWorksheet(wb, list.tables[[table.name]], sheet = table.name)
  }
  
  saveWorkbook(wb)
  
}