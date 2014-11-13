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


pred_ranking <- function (df, response = .(DESERCION),msg=T){
  library(ROCR)
  response_var <- df[[names(response)]]
  df2 <- df[, -which(names(df) == names(response))]
  df2 <- df2[, laply(df2, function(v) {
    if (length(unique(na.omit(v))) == 1) {
      FALSE
    }
    else {
      TRUE
    }
  })]
  res <- ldply(names(df2), function(namevar) {
    if(msg){message(namevar)}
    pred_var <- df[[namevar]]
    daux <- data.frame(response_var = response_var, pred_var = pred_var)
    daux_naomit <- na.omit(daux)
    model <- glm(response_var ~ pred_var, data = daux_naomit, 
                 family = binomial(link = logit))
    pred <- prediction(model$fitted.values, daux_naomit$response)
    perf <- performance(pred, "tpr", "fpr")
    auc <- attr(performance(pred, "auc"), "y.values")[[1]]
    gini <- abs(2*auc-1)
    ks <- max(abs(attr(perf, "y.values")[[1]] - attr(perf,"x.values")[[1]]))
    mxe <- force(attr(performance(pred, "mxe"),"y.values")[[1]])                 #mean cross entropy (validar concordancia)
    return(data.frame(Variable = namevar, AUC = auc, KS = ks,MXE=mxe, GINI=gini,
                      NA.prop = 1 - nrow(daux_naomit)/nrow(daux)))
  })
  res <- res[order(res$AUC, decreasing = TRUE), ]
  res
}

pred_ranking_mr <- function(data,response.name=.(DESERCION)){
  class.res <- unique(data[[response.name]])
  
  df <- ldply(seq(1,length(class.res)),function(ind){
    message(paste("Class:",class.res[ind]))
    daux <- data[,-which(colnames(data)==response.name)]
    daux$response <- 1*(data[[response.name]]==class.res[ind])
    df <- pred_ranking(daux,response=.(response),msg=F)
    df$class <- class.res[ind]
    df
  }, .progress = "text")
  
  df1 <- df %>% group_by(Variable) %>% summarise(AUC = max(AUC),
                                                 KS  = max(KS),
                                                 MXE = min(MXE),
                                                 GINI = max(GINI))
  
  df2 <- reshape(df,timevar = "class", v.names = c("AUC","KS","MXE","GINI"),idvar = "Variable",direction="wide")
  
  df2 <- join(df1,df2,by="Variable",type="left",match="first")
  
  df2 <- df2[,unique(c("Variable",sort(colnames(df2))))]
  
  df2[order(df2$AUC,decreasing = T),]
  
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

conf_matrix <- function(true.values, predictions) {
  t <- table( true = true.values, prediction = predictions)
  # http://www2.cs.uregina.ca/~dbd/cs831/notes/confusion_matrix/confusion_matrix.html
  #                     Prediction
  #                 NegPred   PosPred
  # real NegOutcome
  # real PosOutcome
  AC <- sum(diag(t))/sum(t) #Accuracy (AC) is the he proportion of the total number of predictions that were correct.
  TP <- t[2,2]/sum(t[2,])   #Recall or true positive rate (TP) is the proportion of positive cases that were correctly identified. (BB)
  FP <- t[1,2]/sum(t[1,])   #False positive rate (FP) is the proportion of negatives cases that were incorrectly classified as positive
  TN <- t[1,1]/sum(t[1,])   #True negative rate (TN) is defined as the proportion of negatives cases that were classified correctly (MM)
  FN <- t[2,1]/sum(t[2,])   #False negative rate (FN) is the proportion of positives cases that were incorrectly classified as negative
  P <- t[2,2]/sum(t[,2])    #Precision (P) is the proportion of the predicted positive cases that were correct
  return(list(confusion.matrix = t,
              Accuracy = AC,
              "True Positive rate (BB)" = TP,
              "False Positive rate" = FP,
              "True Negative rate (MM)" = TN,
              "False Negative rate" = FN,
              Precision = P))
}


importance_vars <- function(data,response.name,pred.names = setdiff(colnames(data),response.name),...){
  library(RRF)
  class <- data[[response.name]]
  uniq.c <- unique(class)
  formula <- as.formula(paste("response.aux", paste(pred.names, collapse="+"), sep="~"))
  
  df <- ldply(1:length(uniq.c), function(v,...){
    message(paste("CLASS: ",uniq.c[v]))
    var <- factor(1*(class==uniq.c[v]),levels=c(0,1))
    daux <- subset(data, select = pred.names)
    daux$response.aux <- var
    
    daux <- char2factor(daux)
    daux <- na.roughfix(daux)
    rrf <- RRF(formula, data = daux)
    imp <- data.frame(Variable = rownames(RRF::importance(rrf)),
                      class = uniq.c[v],
                      Mdg = as.numeric((RRF::importance(rrf))))
    imp
  },.progress="text")
  
  df <- reshape(df, timevar = "class", v.names = "Mdg",idvar = "Variable",direction="wide")
  df$Mdg_combinado <- apply(df[,-1],1,max)
  
  daux <- char2factor(data)
  daux <- na.roughfix(daux)
  formula <- as.formula(paste(response.name, paste(pred.names, collapse="+"), sep="~"))
  tree_c <- RRF(formula,daux)
  #tree_c <- tuneRRF(daux[[response.name]], daux[,-which(colnames(daux)==response.name)]),ntreeTry = 1000, stepFactor=1.5, doBest = T, plot=F,trace=T)
  
  df_c <- RRF::importance(tree_c)
  df2 <- data.frame(Variable = rownames(df_c), Mdg.Multiple=as.numeric(df_c))
  
  df <- join(df,df2,by="Variable",type="left",match="first")
  vars <- c("Variable","Mdg.Multiple","Mdg_combinado")
  df <- subset(df,select = unique(c(vars,colnames(df))))
  
  df <- df[order(df$Mdg_combinado,decreasing = T),]
  
  df[,-1] <- round(df[,-1],digits=3)
  df
}
