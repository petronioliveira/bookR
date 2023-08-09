# Pacotes necessários

pacman::p_load(datarium, rstatix, ggpubr)

data("selfesteem2")

# Determinar se existe uma interação significativa entre dieta e o tempo sobre 
# os escoress de autoestima 
 
# Converter o conjunto de dados de amplo para longo

selfesteem2 <- selfesteem2 %>% 
  gather(key = "time",
         value = "score", t1, t2, t3) %>% 
  convert_as_factor(id, time)

selfesteem2 %>% 
  sample_n_by(treatment, time, size = 1)

View(selfesteem2)

selfesteem2 %>%
  rstatix::group_by(time) %>% 
  rstatix::get_summary_stats(score, type = "mean_sd")

# Pressupostos
## Outliers

selfesteem2 %>% 
  group_by(treatment) %>% 
  identify_outliers(score)

## Normalidade

selfesteem2 %>% 
  group_by(treatment,time) %>% 
  shapiro_test(score)

ggqqplot(selfesteem2, "score", ggtheme = theme_bw()) +
  facet_grid(time~treatment, labeller = "label_both")

## Esfericidade

# junto com o teste ANOVA

# Anova

res.aov <- anova_test(data = selfesteem2, 
                      dv = score,
                      wid = id,
                      within = c(treatment, time))
             
get_anova_table(res.aov)

# Uma interação significativa de dois fatores indica que o impacto que um fator
# (por ex., tratamento) tem sobre a variável desfecho (por ex., escore de autoestima) 
# depende do nívek do poutro fator (por ex., time) e vice-versa.
# Dessa forma, pode-se dividir uma interação significativa de dois fatores em:
## EFEITO PRINCIPAL SIMPLES: executar um modelo de uma via da primeira variável 
## (por ex., tratamento) em cada nível da segunda variável (por ex., time).
## COMPARAÇÕES PAREADAS SIMPLES: se o efeito principal simples é significativo.
## executar múltiplas comparações pareadas para determinar quais grupos são diferentes.

# POST HOC: Tratamento
# Efeito do tratamento em cada ponto do tempo

one.way <- selfesteem2 %>% 
  group_by(time) %>% 
  anova_test(dv =score, wid = id, within = treatment) %>% 
  get_anova_table() %>% 
  adjust_pvalue(method = "bonferroni")
one.way

# O efeito principal simples do tratamento não foi significativo no tempo 1, mas 
# foi no tempo 2 e 3

# Comparação de pares entre os grupos de tratamento

pwc <- selfesteem2 %>% 
  group_by(time) %>% 
  pairwise_t_test(
    score ~ treatment, paired = TRUE,
    p.adjust.method = "bonferroni")
pwc
  

# A média do escore de autoestima foi significativamente diferente entre o controle
# e dieta nos tempos 2 e 3.

# POST HOC: Tempo
# Efeito do tempo em cada ponto do tratamento

one.way2 <- selfesteem2 %>% 
  group_by(treatment) %>% 
  anova_test(dv =score, wid = id, within = time) %>% 
  get_anova_table() %>% 
  adjust_pvalue(method = "bonferroni")
one.way2

# Comparação de pares entre os pontos do tempo

pwc2 <- selfesteem2 %>% 
  group_by(treatment) %>% 
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni")
pwc2

# O efeito do tempo é somente significativo para o grupo controle. Todas as 
# comparações foram significativas