freqtable <- function(x, sort.by.freq = FALSE, add.total = FALSE){
  library(plyr)
  library(dplyr)
  
  freqt <- data.frame(Class = addNA(x)) %>%
    group_by(Class) %>%
    summarise(Freq = n())
    
  if(sort.by.freq){
    freqt <- freqt  %>% arrange(-Freq)
  } else {
    freqt <- freqt  %>% arrange(Class)
  }
  
  freqt <- freqt %>%
    mutate(FreqRel = Freq/sum(Freq),
           FreqCum = cumsum(Freq),
           FreqRelCum = cumsum(Freq)/sum(Freq))
    
  if(add.total){
    sums <- data.frame(Class="Total" ,t(colSums( freqt[, c("Freq", "FreqRel")  ] )))
    freqt <- rbind.fill(freqt, sums)  
  }
  
  return(as.data.frame(freqt))

}

freqtable2 <- function(x, y, add.total = FALSE){
  
  freqt <- as.data.frame.matrix(table(x, y), row.names = NULL)
  
  if(add.total){
    t <- cbind(freqt, TotalRow = rowSums(freqt))
    t <- rbind(freqt, TotalCol = colSums(freqt))
  }  
  
  freqt <- cbind(values = rownames(freqt), freqt)
  rownames(freqt) <- NULL
  
  return(freqt)
  
}