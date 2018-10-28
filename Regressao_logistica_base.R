library(dplyr)
library(MASS)
base = readRDS("base_final.rds")

# Selecionamos apenas as variáveis de interesse
# Anteriormente vimos que algumas variáveis atrapalhavam o modelo,
# então decidimos retirá-las.
base_final = base %>% select(-cod_municipio, -Nome_Municipio.x, -Nome_Municipio.y,
                             -FLAG_REALIZOU_CONF_HAB, -FLAG_CONSELHO_TRANS,
                             -disc_ling_estrangeira, -disc_ciencias, -disc_artes,
                             -disc_ling_portuguesa, -disc_ciencias_humanas,
                             -disc_educacao_fisica, -disc_ensino_religioso,
                             -disc_hist_geo, -disc_matematica, 
                             -perc_banheiro_dentro_predio)

# Escolhendo grupo de treino e grupo de teste
train.data <- base_final %>% sample_frac(0.8)
test.data <- base_final %>% anti_join(train.data, by = names(base_final))
# model <- glm(Resposta ~., data = train.data, family = binomial) %>%
#   stepAIC(trace = TRUE, direction = "backward")

# Tratando variáveis para o modelo
x = as.matrix(train.data[,-(1:2)])
y = as.double(train.data[,2])


library(glmnet)

# Modelo de regressão logística usando LASSO e erro médio absoluto como perda
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial", type.measure = "mae")
plot(cv.lasso)


plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
best_lambda = cv.lasso$lambda.1se
lasso_coef <- cv.lasso$glmnet.fit$beta[,cv.lasso$glmnet.fit$lambda == best_lambda]

# Gráfico dos coeficientes do modelo
coef = data.table(lasso = lasso_coef,
                  feature = names(lasso_coef))
to_plot = melt(coef, id.vars = "feature", variable.name = "model", 
               value.name = "coefficient")
ggplot(to_plot, aes(x = feature, y = coefficient, fill = model)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  facet_wrap(~model) +
  guides(fill = F)

# Modelo final
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.1se)


# Testando o ajuste
x.test = as.matrix(test.data[,-(1:2)])
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0, 1, 0)
observed.classes <- test.data$Resposta
accuracy = mean(predicted.classes == observed.classes)
accuracy



