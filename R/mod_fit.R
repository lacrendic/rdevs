# rm(list=ls())

mod_fit <- function(formula, data, model.name = c("logistic", "svm", "ctree", "ada"), subset){

  if(!is.formula(formula)) stop("The parameter formula is not a formula")
  
  response.name <- as.character(formula)[2]
  data[[response.name]] <- as.factor(data[[response.name]])
  response <- as.numeric(as.character(data[[response.name]]))
  
  if(as.character(formula)[3] == "."){
    nvars <- ncol(data) - 1
  } else {
    nvars <- length(unlist(str_split(as.character(formula)[3], "\\s\\+\\s")))
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
    library(plyr)
    model <- ctree(formula, data = data)
    preds <- laply(predict(model, newdata=data, type="prob"), function(x){x[2]})
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

# data(iris)
# data <- iris
# data$Species <- ifelse(data$Species=="virginica", 0, 1)
# head(data)

# formula <- Species ~ .
# formula2 <- Species ~ Petal.Length + Sepal.Length

# model.name <- "svm"
# model.name2 <- "logistic"
# model.name3 <- "ctree"

# subset <- sample(seq(nrow(data)), size = 70)


# mod_fit(formula , data, model.name )
# mod_fit(formula2, data, model.name2)
# mod_fit(formula2, data, model.name3)

# mod_fit(formula2 , data, model.name, subset)
# mod_fit(formula, data, model.name2, subset)
# mod_fit(formula, data, model.name3, subset)




# ################
# #### mod_ad ####
# #   mod_ad <- rpart(formula_all, data = df_mod, method="class")
# #   ss <- summary_predictions(predict(mod_ad,newdata=df_val, type="prob")[,2],
# #                             as.numeric(as.character(df_val$DESERCION)))
# #   ss <- cbind(iter = nvars, modelo = "Árbol de Decisión rpart" , ss)
# #   score_result <- rbind(score_result, ss)


# #################
# #### mod_ad2 ####
# mod_ad2 <- ctree(formula_all, data = df_mod)
# ss <- summary_predictions(laply(predict(mod_ad2, newdata=df_val, type="prob"), function(x){x[2]}),
#                           as.numeric(as.character(df_val$DESERCION)))
# ss <- cbind(iter = nvars, modelo = "Árbol de Decisión ctree" , ss)
# score_result <- rbind(score_result, ss)

# #################
# #### mod_ada ####
# mod_ada <- ada(formula_all, data = df_mod, type='discrete', iter=100)
# ss <- summary_predictions(predict(mod_ada, newdata=df_val, type="probs")[,2],
#                           as.numeric(as.character(df_val$DESERCION)))
# #   save(mod_ada, file="../output/modelo_ada_11_ipll_cftll.RData")
# #   ss1 <- summary_predictions(predict(mod_ada, newdata=df_aux, type="probs")[,2][df_aux$Institucion == "IPLL" & df_aux$Año == 2012],
# #                              as.numeric(as.character(df_aux$DESERCION))[df_aux$Institucion == "IPLL" & df_aux$Año == 2012])
# #   ss2 <- summary_predictions(predict(mod_ada, newdata=df_aux, type="probs")[,2][df_aux$Institucion != "IPLL" & df_aux$Año == 2012],
# #                              as.numeric(as.character(df_aux$DESERCION))[df_aux$Institucion != "IPLL" & df_aux$Año == 2012])
# #   ss <- rbind(cbind(Institucion = "IPLL", ss1), cbind(Institucion = "CFTLL", ss2))
# #   writetable(ss, "../output/score_results_ipll_cftll_mod_ada_12_ipll_cftll.xlsx")

# ss <- cbind(iter = nvars, modelo = "AdaBoost" , ss)
# score_result <- rbind(score_result, ss)

# rm(mod_rf, mod_svm, mod_rl, mod_ad, mod_ad2, mod_ada)