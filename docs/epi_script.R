pacman::p_load(BiocManager,
               car,
               caret,
               DescTools,
               dplyr,
               epiR,
               epitools,
               ggplot2,
               forcats,
               kableExtra,
               limma,
               mice,
               MASS,
               MKmisc,
               mlbench,
               performance,
               pROC,
               readxl,
               sjPlot,
               survival,
               survminer,
               vcd)



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

# Visualizando o tamanho das amostras
dim(dadosTreino)
dim(dadosTeste)

# Explorando a estrutura dos dadosTreino
str (dadosTreino)
str(dadosTeste)

modelo1 <- glm(sobreviveu ~., 
               data = dadosTreino, 
               family = binomial(link = "logit"))

summary(modelo1)

anova(modelo1, test='Chisq')

odds_ratio1 <- round (exp(cbind(OR = coef(modelo1), confint(modelo1))), 3)
print(odds_ratio1)

library(sjPlot)
forest_raw <- plot_model(modelo1)
forest_std <- plot_model(modelo1, type = "std")

library(patchwork)
wrap_plots(forest_raw, forest_std, nrow = 2)

modelo2 <- glm(sobreviveu ~ classe + sexo + idade + irco, family = binomial(link = 'logit'), data=dadosTreino)
summary(modelo2)

odds.ratio2 = exp(cbind(OddsRatio = coef(modelo2),confint(modelo2)))

aic <- AIC(modelo1, modelo2)
aic

anova(modelo1, modelo2, test = "Chisq")
sexo = forcats::fct_rev(sexo)

dadosTreino %>%
  mutate(sobreviveu = ifelse(sobreviveu == 1, 1, 0)) %>%
  group_by(sexo, classe) %>%
  summarise(sobreviveu = mean(sobreviveu)) %>%
  ggplot(aes(x = sexo, y = sobreviveu, color=classe)) +
           geom_line(aes(group = classe)) +
           geom_point() +
           scale_colour_brewer(palette = "Set1") +
           theme_bw()
   
dadosTreino %>%
  mutate(sobreviveu = ifelse(sobreviveu == 1, 1, 0),
         sexo = forcats::fct_rev(sexo)) %>%
  group_by(sexo, classe) %>%
  summarise(sobreviveu = mean(sobreviveu)) %>%
  ggplot(aes(x = sexo, y = sobreviveu, color = classe)) +
  geom_line(aes(group = classe)) +
  geom_point() +
  scale_colour_brewer(palette = "Set1") +
  theme_bw()



# Converter a variável "sobreviveu" para numérica, se necessário
dadosTreino <- dadosTreino %>%
  mutate(sobreviveu = as.numeric(sobreviveu)) %>% 
  mutate(sobreviveu = ifelse(sobreviveu == 1, 0, 1))

# Calcular taxa de sobrevivência por classe e sexo
dadosResumo <- dadosTreino %>%
  group_by(sexo, classe) %>%
  summarise(taxa_sobrevivencia = mean(sobreviveu, na.rm = TRUE))

# Criar gráfico de linha
ggplot(dadosResumo, aes(x = sexo, y = taxa_sobrevivencia, color = classe, group = classe)) +
  geom_line(size = 0.6, linetype = "dashed") +
  geom_point(size = 2) +
  #scale_y_reverse() +
  labs(title = "",
       x = "Sexo",
       y = "Taxa de Sobrevivência",
       color = "Classe") +
  theme_bw()

dadosTreino %>%
  filter(classe == "1", sexo == "fem") %>%
  summarise(taxa_sobrevivencia = mean(as.numeric(sobreviveu), na.rm = TRUE))

modelo3 <- glm(sobreviveu ~ sexo * classe + idade + irco, 
               data = dadosTreino, family = binomial)
summary(modelo3)


modelo2$null.deviance
modelo2$deviance 

estatistica_G <- modelo2$null.deviance - modelo2$deviance 
estatistica_G

# Graus de liberdade para o teste G
# df = (número de parâmetros no modelo) - (número de parâmetros no modelo nulo)

df_nulo <- modelo2$df.null
df_modelo <- modelo2$df.residual
df_G <- df_nulo - df_modelo
print(paste("Graus de Liberdade para o Teste G:", df_G))

# P-valor para o teste G
valor_P <- format(1 - pchisq(estatistica_G, df = df_G), sientific = TRUE)
valor_P

print(paste("P-valor do Teste G:", valor_P))

# Criar um dataframe com os valores de deviance

modelo2$null.deviance 

n_obs <- nrow(dadosTreino)
cat("Tamanho amostral:", n_obs, "\n")

efeitos_modelo <- tidy(modelo2, exponentiate = TRUE, conf.int = TRUE)

# Exibir tabela com odds ratio e intervalos de confiança
print(efeitos_modelo)

dados_plot <- data.frame(
Variavel = c("Classe 2", "Classe 3", "Sexo Masculino", "Idade", "Preço do Bilhete"),
OR = c(0.327, 0.122, 0.0285, 0.969, 0.635),
CI_low = c(0.190, 0.0733, 0.0185, 0.955, 0.503),
CI_high = c(0.557, 0.200, 0.0430, 0.983, 0.781))

# Criar o forest plot
ggplot(dados_plot, aes(x = Variavel, y = OR)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() + # Inverte eixo para melhor leitura
  labs(title = "Forest Plot - Odds Ratio e Intervalos de Confianca",
       x = "Variaveis",
       y = "Odds Ratio") +
  theme_minimal()

print(unique(dadosTreino$classe))  # Verifica se existe




exp(cbind(OddsRatio = coef(modelo2),confint(modelo2)))

dadosGrafico <- data.frame(
  Modelo = c("Nulo", "Ajustado"),
  Deviance = c(modelo2$null.deviance, modelo2$deviance)
)

# Criar o gráfico
ggplot(dadosGrafico, aes(x = Modelo, y = Deviance, fill = Modelo)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Comparacao da Deviance dos Modelos",
       x = "Modelo",
       y = "Deviance") +
  theme_bw() +
  theme(legend.position = "none")





modelo3$null.deviance = 1210.44
modelo3$deviance = 689.33
