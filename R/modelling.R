ks <- function(predictions, labels){
  require(ROCR)
  pred <- prediction(predictions,labels)
  perf <- performance(pred,"tpr","fpr")
  ks <- max(abs(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]]))
  return(c(ks = as.numeric(ks)))
}

aucroc <- function(predictions, labels){
  require(ROCR)
  pred <- prediction(predictions,labels)
  perf <- performance(pred,"tpr","fpr")
  aucroc <- attr(performance(pred,"auc"),"y.values")[[1]]
  return(c(aucroc = aucroc))
}

gini <- function(predictions, labels){
  return(c(gini = 2*as.numeric(aucroc(predictions, labels)) - 1))
}

divergence <- function(predictions, labels){
  s.good <- predictions[labels == 1]
  s.bad <- predictions[labels == 0]
  return(c(divergence = (mean(s.good) - mean(s.bad))^2/(var(s.good) + var(s.bad))*2))
}

gain <- function(predictions, labels, percents = c(0.10, 0.20, 0.30, 0.40, 0.50)){
  require(scales)
  g <- ecdf(predictions[labels==0])(quantile(predictions,percents))
  names(g) <- percent(percents)
  g
}

summary_predictions <- function(predictions,labels){ 
  
  res <- c(N = length(predictions),
           N.good = length(predictions[labels == 1]),
           N.bad = length(predictions[labels == 0]),
           mean = length(predictions[labels == 0])/length(predictions),
           ks(predictions,labels),
           aucroc(predictions,labels),
           gini(predictions,labels),
           divergence(predictions,labels),
           gain = gain(predictions,labels))
  res <- data.frame(t(res))
  names(res) <- gsub("\\.", "", names(res))
  res
}

oddstable <- function(predictions, labels, min = min(predictions), max = max(predictions), cuts = NULL,
                      nclass = 10, round = 0, quantile = T, format.2 = T){
  if(missing(cuts) & quantile){
    cuts <- unique(round(quantile(predictions, seq( 0, 1, length = nclass + 1)), digits=round))
  } 
  if(missing(cuts) & !quantile) {
    cuts <- unique(round(seq( from = min, to = max, length = nclass + 1), digits=round))
  }
  
  t <- table(cut(predictions, cuts, include.lowest=T), labels)
  t <- t[(length(cuts)-1):1,]
  nclass <- dim(t)[1]
  N <- sum(t)
  t2 <- data.frame(class  = row.names(t),
                   n      = (t[,1]+t[,2]),
                   p      = (t[,1]+t[,2])/N,
                   p_acum = cumsum((t[,1]+t[,2])/N),
                   p_desacum  = c(1,((sum(t[,1]+t[,2])-cumsum(t[,1]+t[,2]))/N)[1:(nclass-1)]),
                   n_bad  = t[,1],
                   p_bad  = t[,1]/sum(t[,1]),
                   p_bad_acum = cumsum(t[,1]/sum(t[,1])),
                   p_bad_desacum = c(1,((sum(t[,1])-cumsum(t[,1]))/sum(t[,1]))[1:(nclass-1)]),
                   br     = t[,1]/(t[,1]+t[,2]),
                   br_acum= cumsum(t[,1])/cumsum((t[,1]+t[,2])),
                   br_desacum = c((cumsum(t[,1])/cumsum((t[,1]+t[,2])))[nclass],((sum(t[,1])-cumsum(t[,1]))/(sum(t[,1]+t[,2])-cumsum(t[,1]+t[,2])))[1:(nclass-1)]),
                   odds   =  t[,2]/ t[,1])
  rownames(t2) <- NULL
  
  if(format.2) t2$class <- paste(c(1,cuts[2:(nclass)]+1),c(cuts[2:nclass],999), sep = "-")[nclass:1]
  
  #oddstable
  list(oddstable = t2, cuts = cuts)
  
}


pred_ranking <- function(df, response = .(desercion_1)){
  library(ROCR)
  response_var <- df[[names(response)]]
  df2 <- df[,-which(names(df)==names(response))]
  df2 <- df2[,laply(df2, function(v){ if(length(unique(v))==1){ FALSE } else { TRUE } })]
  
  res <- ldply(names(df2), function(namevar){
    message(namevar)
    pred_var <- df[[namevar]]
    daux <- data.frame(response_var = response_var, pred_var = pred_var)
    daux_naomit <- na.omit(daux)
    
    model <- glm(response_var ~ pred_var, data = daux_naomit, family = binomial(link = logit))
    
    pred <- prediction(model$fitted.values, daux_naomit$response)
    perf <- performance(pred, "tpr","fpr")
    
    auc <- attr(performance(pred,"auc"),"y.values")[[1]]
    ks <- max(abs(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]]))
    
    return(data.frame(Varible = namevar, AUCROC = auc, KS = ks, NA.prop = 1-nrow(daux_naomit)/nrow(daux), log = ""))  
  }, .progress="text")
  res <- res[order(res$AUCROC, decreasing=TRUE),]
  res
}
