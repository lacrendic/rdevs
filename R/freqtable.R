freqtable <- function(variable, variable2, sort.by.count = FALSE, pretty = FALSE, add.total = TRUE){
  
  # variable <- sample(c(letters,NA), size = 1234, prob= sample(1:(length(letters)+1)), replace=TRUE)
  
  if(missing(variable2)){
    if(any(is.na(variable))){
      freq <- table(variable, useNA="always")
    } else {
      freq <- table(variable)
    }
    
    
    table <- data.frame(variable = names(freq),
                        freq = as.vector(freq),
                        cumfreq = cumsum(freq),
                        relfreq = as.vector(prop.table(freq)),
                        row.names = NULL,
                        stringsAsFactors = FALSE)
    
    if(sort.by.count){
      table <- table[order(table$freq, decreasing=T),]
    } 
    
    table$cumrelfreq <- cumsum(table$relfreq)
    
    if(add.total){
      d <- data.frame("Total", sum(table$freq), NA, 1, NA, stringsAsFactors=FALSE)
      names(d) <- names(table)
      table <- rbind(table, d)  
    }
    
    
    if(pretty){
      require(scales)
      table$freq <- prettyNum(table$freq, big.mark=",")
      table$cumfreq <- prettyNum(table$cumfreq, big.mark=",")
      table$relfreq <- percent(table$relfreq)
      table$cumrelfreq <- percent(table$cumrelfreq) 
    }
    
	names(table)[1] <- "category"
    return(table)
    
  } else {
   
    table <- as.data.frame.matrix(table(variable, variable2))
    table <- cbind(data.frame(variable=rownames(table), stringsAsFactors=FALSE), table)
    rownames(table) <- NULL
    
    if(sort.by.count){
      table <- table[order(rowSums(table[,-1])), ]
    }
    
    if(add.total){
      table <- rbind(table, data.frame(variable = "Total.col", t(colSums(table[,-1]))))
      table$Total.row <- rowSums(table[,-1])
      table[nrow(table), ncol(table)] <- NA 
    }
    
    if(pretty){
      require(scales)
      table[,-1] <- prettyNum(table[,-1], big.mark=",")
    }
	
    names(table)[1] <- "categories"
    return(table)
  }
  
}