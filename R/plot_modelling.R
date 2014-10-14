plot_roc <- function(predictions, labels){
  require(ROCR)
  require(ggplot2)
  pred <- prediction(predictions, labels)
  perf <- performance(pred,"tpr","fpr")
  auc <- attr(performance(pred,"auc"),"y.values")[[1]]
  
  df <- data.frame(x = unlist(perf@"x.values") , y = unlist(perf@"y.values"))
  
  p <- ggplot(df, aes(x, y))  + geom_line(size = 1.2, colour = "darkred")
  p <- p + geom_path(data= data.frame(x = c(0,1), y = c(0,1)), colour = "gray", size = 0.7)
  p <- p + scale_x_continuous("False Positive Rate (1 - Specificity)", labels = percent_format(), limits = c(0, 1))
  p <- p + scale_y_continuous("True Positive Rate (Sensivity or Recall)", labels = percent_format(), limits = c(0, 1))
  p
}

plot_gain <- function(predictions, labels){
  require(ROCR)
  require(ggplot2)
  df <- data.frame(percentiles = seq(0, 1, length = 100),
                   gain = gain(predictions, labels , seq(0, 1, length = 100)))
  
  p <-  ggplot(df, aes(percentiles, gain))  + geom_line(size = 1.2, colour = "darkred")
  p <- p + geom_line(aes(x = c(0,1), y = c(0,1)), colour = "gray", size = 0.7)
  p <- p + scale_x_continuous("Sample Percentiles", labels = percent_format(), limits = c(0, 1))
  p <- p + scale_y_continuous("Cumulative Percents of Bads", labels = percent_format(), limits = c(0, 1))
  p
}