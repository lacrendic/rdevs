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

supervised_clust <- function(response, variable, name.output, ...){
  
  library(party)
  library(plyr)
  
  if(!is.numeric(variable)) variable <- as.factor(variable)
  
  daux <- data.frame(response = response, variable = variable)
  
  tree <- ctree(factor(response)~variable, data = daux, ...)  
  plot(tree)
  
  daux$newvar <- factor(tree@where)
  levels(daux$newvar) <- paste0("CLUS_", 1:length(unique(tree@where)))
  
  d <- as.data.frame.matrix(table(daux$variable,daux$newvar))
  
  if(is.numeric(variable)){
    res <- llply(d, function(x) c(min = min(as.numeric(rownames(d)[x!=0])),
                                  max = max(as.numeric(rownames(d)[x!=0]))))
    
    cod <- ldply(res, function(x) data.frame(x[[1]],x[[2]]))
    names(cod) <- c("Cluster", "Minima", "Maxima")
    
  } else{
    res <- llply(d, function(x) rownames(d)[x!=0])
    cod <- ldply(d, function(x) data.frame(rownames(d)[x!=0]))
    names(cod) <- c("Cluster", "Element")
    
  }
  
  # RData
  if(!missing(name.output)) save(daux$newvar, cod, res, file=name.output)
  
  list(newvar = daux$newvar, cod = cod, cod2 = res)
}