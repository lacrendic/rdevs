data(iris)
data(credit)

# data <- iris
# formula1 <- Species ~ .
# formula2 <- Species ~ Petal.Length + Sepal.Length
# data$Species <- ifelse(data$Species=="virginica", 0, 1)
# head(data)

data <- head(credit, 10000)
data <- data[,laply(data, .fun = function(x){ length(unique(x)) != 1})]
data <- chart

formula1 <- bad ~ .
formula2 <- bad ~ sex + age + personal_net_income




model.name1 <- "logistic"
model.name2 <- "ctree"
model.name3 <- "svm"
model.name4 <- "ada"

subset <- sample(seq(nrow(data)), size = round(nrow(data)*0.7))


mod_fit(formula1, data, model.name1)
mod_fit(formula2, data, model.name1)
mod_fit(formula1, data, model.name1, subset)
mod_fit(formula2, data, model.name1, subset)

mod_fit(formula1, data, model.name2)
mod_fit(formula2, data, model.name2)
mod_fit(formula1, data, model.name2, subset)
mod_fit(formula2, data, model.name2, subset)

mod_fit(formula1, data, model.name3)
mod_fit(formula2, data, model.name3)
mod_fit(formula1, data, model.name3, subset)
mod_fit(formula2, data, model.name3, subset)





################
#### mod_ad ####
#   mod_ad <- rpart(formula_all, data = df_mod, method="class")
#   ss <- summary_predictions(predict(mod_ad,newdata=df_val, type="prob")[,2],
#                             as.numeric(as.character(df_val$DESERCION)))
#   ss <- cbind(iter = nvars, modelo = "Árbol de Decisión rpart" , ss)
#   score_result <- rbind(score_result, ss)


#################
#### mod_ad2 ####
mod_ad2 <- ctree(formula_all, data = df_mod)
ss <- summary_predictions(laply(predict(mod_ad2, newdata=df_val, type="prob"), function(x){x[2]}),
                          as.numeric(as.character(df_val$DESERCION)))
ss <- cbind(iter = nvars, modelo = "Árbol de Decisión ctree" , ss)
score_result <- rbind(score_result, ss)

#################
#### mod_ada ####
mod_ada <- ada(formula_all, data = df_mod, type='discrete', iter=100)
ss <- summary_predictions(predict(mod_ada, newdata=df_val, type="probs")[,2],
                          as.numeric(as.character(df_val$DESERCION)))
#   save(mod_ada, file="../output/modelo_ada_11_ipll_cftll.RData")
#   ss1 <- summary_predictions(predict(mod_ada, newdata=df_aux, type="probs")[,2][df_aux$Institucion == "IPLL" & df_aux$Año == 2012],
#                              as.numeric(as.character(df_aux$DESERCION))[df_aux$Institucion == "IPLL" & df_aux$Año == 2012])
#   ss2 <- summary_predictions(predict(mod_ada, newdata=df_aux, type="probs")[,2][df_aux$Institucion != "IPLL" & df_aux$Año == 2012],
#                              as.numeric(as.character(df_aux$DESERCION))[df_aux$Institucion != "IPLL" & df_aux$Año == 2012])
#   ss <- rbind(cbind(Institucion = "IPLL", ss1), cbind(Institucion = "CFTLL", ss2))
#   writetable(ss, "../output/score_results_ipll_cftll_mod_ada_12_ipll_cftll.xlsx")

ss <- cbind(iter = nvars, modelo = "AdaBoost" , ss)
score_result <- rbind(score_result, ss)

rm(mod_rf, mod_svm, mod_rl, mod_ad, mod_ad2, mod_ada)