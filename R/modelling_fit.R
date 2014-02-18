mod_fit <- function(formula, data, model.name = c("logistic", "svm", "ctree", "ada"), subset){
  library(plyr)
  char2factor <- function(df) { data.frame(lapply(df, function (v) { if (is.character(v)) factor(v) else v })) }
  if(!model.name %in% c("logistic", "svm", "ctree", "ada")){ stop("model.name not valid") }
  if(!is.formula(formula)) stop("The parameter formula is not a formula")
  
  response.name <- as.character(formula)[2]
  data[[response.name]] <- as.factor(data[[response.name]])
  response <- as.numeric(as.character(data[[response.name]]))
  
  if(as.character(formula)[3] == "."){
    nvars <- ncol(data) - 1
  } else {
    nvars <- length(unlist(strsplit(as.character(formula)[3], "\\s\\+\\s")))
  }
  
  if(!missing(subset)){
    data_val <- data[setdiff(seq(nrow(data)), subset),]
    data <- data[subset,]
  }
  
  if(model.name == "logistic"){
    model <- glm(formula, family=binomial(logit), data = data, na.action = na.omit)
    preds <- predict(model, newdata=data, type="response")
    if(!missing(subset)){
      preds_val <- predict(model, newdata=data_val, type="response")
    }
  } else if(model.name == "svm"){
    library(e1071)
    model <- svm(formula, data = data, na.action = na.omit, probability = TRUE)
    preds <- 1 - attr(predict(model, newdata=data, probability = TRUE), "prob")[,1]
    if(!missing(subset)){
      preds_val <- 1 - attr(predict(model, newdata=data_val, probability = TRUE), "prob")[,1]
    }
  } else if(model.name == "ctree"){
    library(party)
    model <- ctree(formula, data = char2factor(data))
    preds <- laply(predict(model, newdata=char2factor(data), type="prob"), function(x){x[2]})
    if(!missing(subset)){
      preds_val <- laply(predict(model, newdata=data_val, type="prob"), function(x){x[2]})
    }
  }

  indicators <- summary_predictions(preds, as.numeric(as.character(data[[response.name]])))
  
  if(!missing(subset)){
    indicators_val <- summary_predictions(preds_val, as.numeric(as.character(data_val[[response.name]])))
    indicators <- rbind(indicators, indicators_val)
    indicators <- cbind(subset = c("Train", "Test"), indicators)
  }
  
  indicators <- cbind(model = model.name, indicators)
  indicators <- cbind(indicators, Nvars = nvars)
  
  list(model = model, indicators = indicators)
}
