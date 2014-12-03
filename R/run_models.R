run_models <- function(training, testing,
                      response.name, pred.names=setdiff(names(training), response.name),
                      models = c("knn","nnet","rf","ctree","LogitBoost","plr","bayesglm","rl","ada","gbm"),
                      indicator.class="T", listpar){
  library(caret)
  # data(credit)
  # credit$bad <- ifelse(credit$bad, "T", "F")
  # run_models(credit, credit, response.name="bad")
 
  listparo <- list(rf = list(method = "rf", par = data.frame(mtry=3)),
                   rl = list(method = "plr", par = data.frame(cp="aic", lambda=1e-4)),
                   plr = list(method = "plr", par = data.frame(cp="aic", lambda=10)),
                   nnet = list(method = "nnet", par =data.frame(size=5, decay=0.1)),
                   LogitBoost = list(method = "LogitBoost", par =data.frame(nIter=100)),
                   ctree = list(method = "ctree", par =data.frame(mincriterion=0.95)),
                   #evtree = list(method = "evtree", par =data.frame(alpha=1)),
                   bayesglm = list(method = "bayesglm", par =NULL),
                   ada = list(method = "ada", par =data.frame(iter = 100, maxdepth = 10, nu = 1)),
                   knn = list(method = "knn", par =data.frame(k = 1)),
                   gbm = list(method = "gbm", par =data.frame(n.trees=20,interaction.depth=20 , shrinkage=0.5)))
  
  if(!missing(listpar)){
    for(n in names(listpar)){
      listparo[[n]] <- listpar[[n]]
    }
  }
  
  res <- data.frame(nvars = rep(seq(length(pred.names)), each=length(models)),
                    model = rep(models, length(pred.names)), stringsAsFactors=FALSE)
  res <- dlply(res, .variables= .(nvars, model))
  
  res <- ldply(res, function(x){
    message(sprintf("########## Modelo %s con %s vars", x$model, x$nvars))
    t <- Sys.time()   
    f <- as.formula(paste(response.name, paste(pred.names[seq(x$nvars)], collapse=" + "), sep =" ~ "))
    message("\t\t\t\tAjustando")
    if(x$model %in% c("ada","rf") & x$nvars == 1){ return(x) }
    if(!x$model %in% c("ada")){
      # x <- data.frame(nvars=2, model = "gbm", stringsAsFactors=FALSE)
      # f <- as.formula("DESERCION~COMU_RES_TRUNC+DESC_CARRERA")     
      mod <- train(f, method=listparo[[x$model]][["method"]], data = training,
                   tuneGrid=listparo[[x$model]][["par"]], trControl=trainControl(method="none"))
      preds <- predict(mod, newdata=testing, type ="prob")[, 2]
      }else{
      mod<-ada(f,data=training)  
      preds <- predict(mod, newdata=testing, type ="prob")[, 2]
    }
   
    message("\t\t\t\tCalculando Indicadores")
    sp <- summary_predictions(predictions=preds,
                              labels = ifelse(as.character(testing[[response.name]])==indicator.class, 1, 0))
    message(sprintf("\t\t\t\tTiempo %s segundos", round(t <- Sys.time()-t,digits=1)))
    cbind(x, sp, data.frame(tiempo = t))
  })
  
  res <- join(res, data.frame(nvars=seq(pred.names), namevar = pred.names), by="nvars", type="left")
  res$namevar <- factor(res$namevar, levels=pred.names)
  
  res
}

run_models_mr <- function(training, testing, response.name, pred.names = setdiff(colnames(training), response.name),
                          models=c("gbm","lda","multinom","nnet","pda","pls","rbf","rf","treebag"), obj.class="R",fit_control = trainControl(method = "LOOCV"), len.grid = 4,...){
  
  res <- data.frame(nvars = rep(seq(length(pred.names)), each=length(models)),
                    model = rep(models, length(pred.names)), stringsAsFactors=FALSE)
  
  res <- dlply(res, .variables= .(nvars, model))
  
  res2 <- ldply(res,function(x,...){
    #x <- res[[1]]
    message(sprintf("########## Modelo %s con %s vars", x$model, x$nvars))
    
    if(x$model%in%c("gbm","pls") & x$nvars==1){return(data.frame(nvars=x$nvars,model=x$model,N.obs=NA,Accuracy=NA,KS=NA,AUC=NA,Gini=NA,Gain10=NA,Gain20=NA,Gain30=NA,Gain40=NA,Gain50=NA))}
    f <- as.formula(paste(response.name, paste(pred.names[seq(x$nvars)], collapse=" + "), sep =" ~ "))
    message("Ajustando...")
    
    mod <- train(f, training, trControl = fit_control, method = x$model, verbose=F,tuneLength = len.grid, ...)    
    
    message("Calculando indicadores de validación...")
    pred <- predict(mod,newdata = testing, type="prob")
    colnames(pred) <- levels(testing[[response.name]])
    
    sp <-  summary_predictions_mr(predictions = pred, response = testing[[response.name]], imp.class = obj.class)
    message("Modelo listo!")
    return(cbind(x,Accuracy=max(mod$results$Accuracy),sp))

  })
  
  res <- join(res2, data.frame(nvars=seq(pred.names), namevar = pred.names), by="nvars", type="left")
  res$namevar <- factor(res$namevar, levels=pred.names)
  
  res <- res[,-1]
  res[,unique(c("nvars","model","namevar","N.obs","TNegResponse",colnames(res)))]
}


run_models2 <- function(training, testing,
                        response.name, pred.names=setdiff(names(training), response.name),
                        models = c("knn","nnet","rf","ctree","LogitBoost","plr","bayesglm","rl","ada","gbm"),
                        indicator.class="T", listpar,model.tune = F,...){
  library(caret)
 
  listparo <- list(rf = list(method = "rf", par = data.frame(mtry=3)),
                   rl = list(method = "plr", par = data.frame(cp="aic", lambda=1e-4)),
                   plr = list(method = "plr", par = data.frame(cp="aic", lambda=10)),
                   nnet = list(method = "nnet", par =data.frame(size=5, decay=0.1)),
                   LogitBoost = list(method = "LogitBoost", par =data.frame(nIter=100)),
                   ctree = list(method = "ctree", par =data.frame(mincriterion=0.95)),
                   #evtree = list(method = "evtree", par =data.frame(alpha=1)),
                   bayesglm = list(method = "bayesglm", par =NULL),
                   ada = list(method = "ada", par =data.frame(iter = 100, maxdepth = 10, nu = 1)),
                   knn = list(method = "knn", par =data.frame(k = 1)),
                   gbm = list(method = "gbm", par =data.frame(n.trees=20,interaction.depth=20 , shrinkage=0.5)))
  
  if(!missing(listpar)){
    for(n in names(listpar)){
      listparo[[n]] <- listpar[[n]]
    }
  }
  
  res <- data.frame(nvars = rep(seq(length(pred.names)), each=length(models)),
                    model = rep(models, length(pred.names)), stringsAsFactors=FALSE)
  res <- dlply(res, .variables= .(nvars, model))
  
  res <- ldply(res, function(x,...){
    message(sprintf("########## Modelo %s con %s vars", x$model, x$nvars))
    t <- Sys.time()   
    f <- as.formula(paste(response.name, paste(pred.names[seq(x$nvars)], collapse=" + "), sep =" ~ "))
    message("\t\t\t\tAjustando")
    if(x$model %in% c("ada","rf") & x$nvars == 1){ return(x) }
    if(!x$model %in% c("ada")){
      # x <- data.frame(nvars=2, model = "gbm", stringsAsFactors=FALSE)
      # f <- as.formula("DESERCION~COMU_RES_TRUNC+DESC_CARRERA")     
      if(!model.tune){
        mod <- train(f, method=listparo[[x$model]][["method"]], data = training,
                     tuneGrid=listparo[[x$model]][["par"]], trControl=trainControl(method="none"))  
        }else{
          mod <- train(f, method=listparo[[x$model]][["method"]], data = training,...)  
        }
      
      preds <- predict(mod, newdata=testing, type ="prob")[, 2]
      }else{
      mod<-ada(f,data=training)  
      preds <- predict(mod, newdata=testing, type ="prob")[, 2]
    }
   
    message("\t\t\t\tCalculando Indicadores")
    sp <- summary_predictions(predictions=preds,
                              labels = ifelse(as.character(testing[[response.name]])==indicator.class, 1, 0))
    message(sprintf("\t\t\t\tTiempo %s segundos", round(t <- Sys.time()-t,digits=1)))
    cbind(x, sp, data.frame(tiempo = t))
  })
  
  res <- join(res, data.frame(nvars=seq(pred.names), namevar = pred.names), by="nvars", type="left")
  res$namevar <- factor(res$namevar, levels=pred.names)
  
  res
}