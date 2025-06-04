pacman::p_load(ggplot2, dplyr, ggeffects)

dadosTitanic <- readxl::read_excel("Arquivos/dadosTitanic.xlsx")
str(dadosTitanic)
dadosTitanic <- dplyr::select(dadosTitanic, 
                              -c(id, nome, ticket, tarifa, cabine, porto_embarque))

colSums(is.na(dadosTitanic))
dados <- mice::mice(dadosTitanic, m=5, method="pmm")
dados_completos <- mice::complete(dados, 1) 

colSums(is.na(dados_completos))
str(dados_completos)

dados_completos <- dados_completos %>%
  mutate(sexo = factor(sexo)) %>%
  mutate(classe = factor(classe)) %>% 
  mutate(sobreviveu = factor(sobreviveu))

str(dados_completos)

# Definindo a semente para reprodutibilidade
set.seed(123)

# Embaralhando os dados
dados_completos <- dados_completos %>% sample_frac(size = 1)

# Definindo o ponto de corte para treino
split_index <- round(0.7 * nrow(dados_completos))

# Dividindo os dados
dadosTreino <- dados_completos %>% slice(1:split_index)
dadosTeste <- dados_completos %>% slice((split_index + 1):n())

levels(dadosTreino$sexo)
levels(dadosTreino$classe)


modelo2 <- glm(sobreviveu ~ classe + sexo + idade + irco, family = binomial(link = 'logit'), data=dadosTreino)
summary(modelo2)

#sobreviveu = factor(sobreviveu, levels = c(0, 1), labels = c("Morreu", "Sobreviveu")),
library(ggeffects)

titanic_df <- dadosTreino %>%
  dplyr::select(- pafi) %>%
  mutate(classe = factor(classe, levels = c(1, 2, 3), 
                         labels = c("1ª Classe", "2ª Classe", "3ª Classe"))) 

modelo3 <- glm(sobreviveu ~ sexo * classe + idade + irco, data = titanic_df, family = "binomial")
summary(modelo3)


pred_effects <- ggpredict(modelo3, terms = c("classe", "sexo"))


plot(pred_effects, show_data = FALSE, connect_lines = T) +
  labs(x = "Classe do Passageiro",
       y = "Probabilidade Predita de Sobrevivência",
       title = "Interação entre Sexo e Classe na Sobrevivência do Titanic", 
       color = "Sexo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right") +
  # Ajustar limites do eixo Y para probabilidades (0 a 1)
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1),
                     expand = expansion(add = c(0,0.05)))

# O gráfico mostrará as probabilidades preditas de sobrevivência (eixo Y) 
# para cada classe (eixo X).
# Você terá duas linhas, uma para cada sexo (Male e Female), conectando 
# as probabilidades preditas dentro de cada classe. 
# Se as linhas são paralelas (ou quase paralelas): Não há uma interação 
# significativa. O efeito de uma variável na sobrevivência é o mesmo 
# (ou muito parecido) em todos os níveis da outra variável. Se as linhas
#  não são paralelas e se cruzam (ou mostram diferentes inclinações/distâncias):
#   Indica uma interação. O efeito de uma variável na sobrevivência depende dos 
#   níveis da outra variável. Por exemplo, a diferença na probabilidade de 
#   sobrevivência entre homens e mulheres pode ser muito diferente na 1ª classe versus a 3ª classe.

# Outro gráfico manual

# 1. Criar um novo data frame com as combinações das variáveis para predição
new_data <- expand.grid(
  sexo = levels(titanic_df$sexo),
  classe = levels(titanic_df$classe),
  idade = mean(dadosTreino$idade, na.rm = TRUE),
  irco = mean(dadosTreino$irco, na.rm = TRUE))

# 2. Obter as probabilidades preditas do modelo
# type = "response" para obter as probabilidades
new_data$predicted_prob <- predict(model_interaction, 
                                   newdata = new_data, 
                                   type = "response")

# 3. Plotar os resultados
ggplot(new_data, aes(x = classe, y = predicted_prob, color = sexo, group = sexo)) +
  geom_line(linewidth = 1.2) + # Adiciona as linhas
  geom_point(size = 3) +     # Adiciona os pontos
  labs(
    x = "Classe do Passageiro",
    y = "Probabilidade Predita de Sobrevivência",
    title = "Interação entre Sexo e Classe na Sobrevivência do Titanic",
    color = "Sexo"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
 
summary(modelo3)
modelo3$coefficients[1]
odds_ratios <- exp(cbind(OddsRatio = coef(modelo3),confint(modelo3)))
odds_ratios[1,2]
odds_ratios[2,1]

aic <- AIC(modelo2, modelo3)
aic
round(aic[1, 2],2) - round(aic[2, 2],2)

prob_pred <- predict(modelo2, type = "response")
prob_pred_int <- predict(modelo3, type = "response")

library(pROC)
roc2<- roc(response = dadosTreino$sobreviveu, predictor = prob_pred) 
roc3 <- roc(response = dadosTreino$sobreviveu, predictor = prob_pred_int)

plot(roc2, 
     main = "Curva ROC", 
     print.auc = TRUE, 
     legacy.axes=TRUE,
     print.auc.y = 0.2,
     ylab="Sensibilidade",
     xlab="1 - Especificidade")

plot(roc3, 
     main = "",
     col="tomato",
     print.auc = TRUE, 
     legacy.axes=TRUE,
     print.auc.y = 0.1,
     add =TRUE)


dadosTeste <- dadosTeste %>%
  dplyr::select(- pafi) %>%
  mutate(classe = factor(classe, levels = c(1, 2, 3), 
                         labels = c("1ª Classe", "2ª Classe", "3ª Classe"))) 
set.seed(234)


pred_prob <- predict(modelo3, newdata = dadosTeste, type = "response")

pred_class <- ifelse(pred_prob > 0.5, 1, 0)
pred_class <- factor(pred_class, levels = c(0, 1))

library(caret)

conf_matrix <- confusionMatrix(pred_class, 
                               dadosTeste$sobreviveu, 
                               positive ="1")
print(conf_matrix)


anova <- anova(modelo2, modelo3, test = "Chisq")
anova
anova$`Pr(>Chi)`


# Probabilidades de predição no conjunto de teste
prob_pred_teste <- predict(modelo3, newdata = dadosTeste, type = "response")

# Probabilidades de predição no conjunto de treino
prob_pred_treino <- predict(modelo3,type = "response")


roc_obj_treino <- roc(response = dadosTreino$sobreviveu, predictor = prob_pred_treino) 
roc_obj_teste <- roc(response = dadosTeste$sobreviveu, predictor = prob_pred_teste) 

auc_valor_treino <- auc(roc_obj_treino)
auc_valor_teste <- auc(roc_obj_teste)


# # Dataframe dos dados treino
dados_roc_treino <- data.frame(
  TFP = 1 - roc_obj_treino$specificities,
  TVP = roc_obj_treino$sensitivities,
  Model = "Treino")

# Dataframe dos dados teste
dados_roc_teste <- data.frame(
  TFP = 1 - roc_obj_teste$specificities,
  TVP = roc_obj_teste$sensitivities,
  Model = "Teste")

dados_roc_combinados <- rbind(dados_roc_treino, dados_roc_teste)

ggplot(dados_roc_combinados, 
       aes(x = TFP, y = TVP, color = Model)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "navy", size = 0.5) +
  labs(title = "Comparação de Curvas ROC",
       x = "Taxa de Falsos Positivos (TFP)",
       y = "Taxa de Verdadeiros Positivos (TVP)",
       color = "Dados") + # Legenda para as cores
  annotate("text", x = 0.6, y = 0.2, label = paste("AUC Teste =", round(auc_valor_teste, 3)),
           color = "tomato", size = 4) +
  annotate("text", x = 0.6, y = 0.1, label = paste("AUC Treino =", round(auc_valor_treino, 3)),
           color = "steelblue", size = 4) +
  scale_color_manual(values = c("Teste" = "tomato",
                                "Treino" = "steelblue")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



library(performance)
plot(check_model(modelo3, residual_type = "normal"))

plot(check_model(modelo2, residual_type = "normal"))


vif_valores2 <- car::vif(modelo3)
print(vif_valores2)

vif_valores <- car::vif(modelo2)
print(vif_valores)

library(DescTools)
PseudoR2(modelo2, which =c("Nagelkerke", "McFadden", "CoxSnell"))
PseudoR2(modelo3, which =c("Nagelkerke", "McFadden", "CoxSnell"))

residuos_p <- rstandard(modelo2)
summary(residuos_p)

residuos_p3 <- rstandard(modelo3)
summary(residuos_p3)

plot(modelo2, which = 5)
plot(modelo3, which = 5)

hat <- hatvalues(modelo2)
summary (hat)
hat3 <- hatvalues(modelo3)
summary (hat3)
