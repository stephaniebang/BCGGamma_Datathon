library(dplyr)
library(MASS)

base <- readRDS("base_final.rds")

# Selecionamos apenas as variaveis de interesse
# Inicialmente vimos que a presença das variaveis de disciplina estavam tendo um efeito negativo
# na modelagem. Como a nota do ideb leva em consideracao portugues e matematica, consideramos apenas estas.
base_final <- base %>% 
  dplyr::select(-cod_municipio, -Nome_Municipio.x, -Nome_Municipio.y,
                -disc_ling_estrangeira, -disc_artes, -disc_educacao_fisica, -disc_ciencias,
                -disc_ciencias_humanas, -disc_hist_geo, -disc_ensino_religioso)

# Escolhendo grupo de treino e grupo de teste
train.data <- base_final %>% sample_frac(0.8)
test.data  <- base_final %>% anti_join(train.data, by = names(base_final))
# model <- glm(Resposta ~., data = train.data, family = binomial) %>%
#   stepAIC(trace = TRUE, direction = "backward")

# Tratando variaveis para o modelo
x = as.matrix(train.data[,-(1:2)])
y = as.double(train.data[,2])


library(data.table)
library(glmnet)

# Modelo de regressao logistica usando LASSO e erro medio absoluto como perda
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial", type.measure = "mae")
plot(cv.lasso)


plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
best_lambda <- cv.lasso$lambda.1se
lasso_coef  <- cv.lasso$glmnet.fit$beta[,cv.lasso$glmnet.fit$lambda == best_lambda]

# Grafico dos coeficientes do modelo
coef <- data.table(lasso   = lasso_coef,
                   feature = names(lasso_coef))

to_plot <- melt(coef, id.vars = "feature", variable.name = "model", 
                value.name = "coefficient")

library(ggplot2)

ggplot(to_plot, aes(x = feature, y = coefficient, fill = model)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  facet_wrap(~model) +
  guides(fill = F)

# Modelo final
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.1se)


# Testando o ajuste
x.test            <- as.matrix(test.data[,-(1:2)])
probabilities     <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0, 1, 0)
observed.classes  <- test.data$Resposta
accuracy          <- mean(predicted.classes == observed.classes)
accuracy

