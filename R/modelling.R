pred_ranking <- function(df, response = .(desercion_1)){
  library(ROCR)
  response_var <- df[[names(response)]]
  df2 <- df[,-which(names(df)==names(response))]
  df2 <- df2[,laply(df2, function(v){ if(length(unique(na.omit(v)))==1){ FALSE } else { TRUE } })]
  
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
    return(data.frame(Variable = namevar, AUCROC = auc, KS = ks, NA.prop = 1-nrow(daux_naomit)/nrow(daux), log = ""))  
  }, .progress="text")
  res <- res[order(res$AUCROC, decreasing=TRUE),]
  res
}

char2factor <- function(df) {
  data.frame(lapply(df, function (v) {
    if (is.character(v)) factor(v)
    else v
  }))
}

pred_ranking_rrf <- function(data, response.name, pred.names = setdiff(names(data), response.name), ...){

  library(RRF)

  formula <- as.formula(paste(response.name, paste(pred.names, collapse="+"), sep= " ~ "))  
  daux <- subset(data, select=c(response.name, pred.names))
  daux <- char2factor(daux)
  daux <- na.roughfix(daux)
  daux[[response.name]] <- factor(daux[[response.name]])
  rrf <- RRF(formula, data=daux, ...)
  imp <- data.frame(variable = rownames(RRF::importance(rrf)), mdg = as.numeric((RRF::importance(rrf))))
  imp <- imp[order(imp$mdg, decreasing=TRUE),]
  imp
}