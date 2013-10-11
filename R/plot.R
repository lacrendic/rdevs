plot_bar <- function(variable, show.values = TRUE, sort.by.count = TRUE, color = "darkred", transpose = FALSE){
  require(ggplot2)
  require(plyr)
  # Remeberber kid, this function depends on 'freqtable' function.
  variable <- ifelse(is.na(variable), "NA", variable)
  t1 <- freqtable(variable, sort.by.count=sort.by.count, add.total=FALSE)
  t2 <- freqtable(variable, sort.by.count=sort.by.count, add.total=FALSE, pretty=TRUE)
  names(t2)[2:5] <- paste("label", names(t2)[2:5], sep="_")
  
  t <- join(t1, t2)
  t$variable <- factor(t$category, levels=t1$category)
  t$id <- seq(nrow(t))
  
  p <- ggplot(t, aes(x = variable)) +
    geom_bar(aes(y = relfreq), stat="identity", fill = "darkred") +
    scale_y_continuous(labels = percent_format())
    
  if(show.values){
    p <- p + scale_y_continuous(labels = percent_format(), limits = c(0,max(t$relfreq)+.1))
    if(transpose){
      p <- p + geom_text(aes(id, relfreq, label = label_freq), size = 4, hjust = -.1, vjust = 0)
      p <- p + geom_text(aes(id, relfreq, label = label_relfreq), size = 4, hjust = -1.5, vjust = 0)
      p
    } else {
      p <- p + geom_text(aes(id, relfreq, label = label_relfreq), size = 4, hjust = .5, vjust = -2.5)                            
      p <- p + geom_text(aes(id, relfreq, label = label_freq), size = 3.8, hjust = .5, vjust = -1)
      
    }   
  }
  
  if(transpose) p <- p + coord_flip()
  
  p <- p + ylab(NULL) + xlab(NULL)
  return(p)
  
}

plot_pie <- function(variable){
  require(ggplot2)
  data <- data.frame(variable=factor(variable))
  ggplot(data, aes(x = factor(1), fill = variable)) + geom_bar(width = 1) + 
    coord_polar(theta = "y") + 
    xlab(NULL) +  theme(axis.ticks = element_blank(), axis.text = element_blank())
}


plot_hist <- function(variable, count = TRUE, color = "#1E90FF"){
  require(ggplot2)
  variable <- na.omit(variable)
  p <- qplot(variable, geom = "blank")
  if(count){
    p <- p + geom_histogram(aes(y = ..count..), fill = color, colour = "black",
                            binwidth=diff(range(variable))/30) + ylab("Count")
  } else {
    p <- p + geom_histogram(aes(y = ..density..), fill = color, colour = "black",
                            binwidth=diff(range(variable))/30) + ylab("Density") 
  }
  p <- p + ylab(NULL) + xlab(NULL)
  
  p
}


plot_density <- function(variable, color = "blue", alpha = 0.6, ...){
  require(ggplot2)
  ggplot(data.frame(variable),aes(variable)) + geom_density(fill=color, alpha = alpha) +
    xlab(NULL) + ylab(NULL) 
}


plot_pareto <- function(variable, prop = TRUE, ...){
  require(ggplot2)
  t <- freqtable(variable, sort.by.count=TRUE, add.total=FALSE)
  t$variable <- factor(t$variable, levels=t$variable)
  t$id <- seq(nrow(t))
  
  ggplot(t) + 
    geom_bar(aes(x=variable, y=relfreq), stat = "identity") +
    geom_line(aes(x=id, y=cumrelfreq)) + 
    geom_point(aes(x=id, y=cumrelfreq)) + 
    scale_y_continuous(labels = percent_format()) +
    xlab(NULL) + ylab(NULL)
  
}