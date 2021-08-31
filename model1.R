
## Analise do diferencial de rendimentos do trabalho por genero
## Modelagem
## Autores: Vitor Hugo Miro e Natalia Carvalho

###--------------------------------------------------------------------------###
# limpar "Area de trabalho" do R
rm(list = ls())
options(scipen = 999) # para evitar notacao cientifica.

###--------------------------------------------------------------------------###

## Variaveis utilizadas na analise
# 'Ano', 'Trimestre', 'UF', 'UPA', 'Estrato', 
# 'V1008', 'V1014', 'V1027', 'V1028',
#'V2007'= sexo
#'V2009'= idade
#'V2010'= cor/raca
#'VD3004'= Escolaridade
#'VD3005'= Anos de estudo
#'VD4001'= Condicao em relacao a forca de trabalho
#'VD4002'= Condicao de ocupacao
#'VD4009'= Cond ocup e cat emprego
#'VD4019'= Rend. todos trabalhos habitual
# 'VD4020'= Rend. todos trabalhos efetivo
# 'VD4031'= Horas habitual trabalhadas em todos os trabalhos
# 'VD4035'= Horas efetivas trabalhadas em todos os trabalhos

###--------------------------------------------------------------------------###

# Carrega dados da PNADC já salvos em RDS 

pnadc <- readRDS("pnadc19.rds")

###--------------------------------------------------------------------------###
# Carrega os pacotes necessários
library(tidyverse)

###--------------------------------------------------------------------------###
# Criar variáveis para análise
pnadc <- pnadc %>% 
    mutate(sexo = factor(case_when(V2007==1 ~ "homem",
                                   V2007==2 ~ "mulher")),
           
           cor = factor(case_when(V2010==1 | V2010==3 ~ "branco",
                                  V2010==2 | V2010==4 | V2010==5 ~ "preto ou pardo")),
           
           eativo = factor(case_when(VD4001==1 ~ "econ.ativo",
                                     VD4001==2 ~ "econ.inativo")),
           
           ocup = factor(case_when(VD4002==1 ~ "ocupado",
                                   VD4002==2 ~ "desocupado")),
           
           ocup_cat = factor(case_when(VD4009==01 ~ "setor privado com carteira",
                                       VD4009==02 ~ "setor privado sem carteira",
                                       VD4009==03 ~ "domestico com carteira",
                                       VD4009==04 ~ "domestico sem carteira",
                                       VD4009==05 ~ "setor publico com carteira",
                                       VD4009==06 ~ "setor publico sem carteira",
                                       VD4009==07 ~ "militar e estatutario",
                                       VD4009==08 ~ "empregador",
                                       VD4009==09 ~ "conta propria",
                                       VD4009==10 ~ "familiar")),
           
           educ = factor(case_when(VD3004==1 ~ "sem instrução",
                                   VD3004==2 ~ "fundamental incomp",
                                   VD3004==3 ~ "fundamental comp",
                                   VD3004==4 ~ "médio incomp",
                                   VD3004==5 ~ "médio comp",
                                   VD3004==6 ~ "superior incomp",
                                   VD3004==7 ~ "superior comp")),
           
           regiao = factor(case_when(UF %in% 10:19 ~ "Norte",
                                     UF %in% 20:29 ~ "Nordeste",
                                     UF %in% 30:39 ~ "Sudeste",
                                     UF %in% 40:49 ~ "Sul",
                                     UF %in% 50:59 ~ "Centro Oeste")) )

pnadc <- pnadc %>% rename(idade = V2009,
                          anos_estudo = VD3005,
                          rendef = VD4020,
                          horas = VD4035)

pnadc <- pnadc %>% mutate(lrend = log(rendef),
                          rendh = rendef/horas,
                          lrendh = log(rendh+1))

###--------------------------------------------------------------------------###
str(pnadc)

# Filtro para idade e situação de ocupação
pnadc_ocup <- filter(pnadc, idade >=25 & ocup == "ocupado")


# Contando missing values no DF
na <- pnadc_ocup %>% 
    select(everything()) %>% 
    summarise_all(funs(sum(is.na(.))))

sum(is.na(pnadc_ocup$rendef))

# Rendim. médio do trabalho para amostra
pnadc_ocup %>%
    group_by(sexo) %>%
    summarise(mean = mean(rendef, na.rm = TRUE), n())

pnadc_ocup %>%
    group_by(sexo) %>%
    summarise(mean = mean(rendh, na.rm = TRUE), n())

# Quantis dos salarios x sexo
pnadc_ocup %>% 
    summarize(quants = quantile(rendef,
                                probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
                                na.rm = TRUE))

pnadc_ocup %>%
    group_by(sexo) %>%
    summarize(quants = quantile(rendef,
                                probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
                                na.rm = TRUE))

pnadc_ocup <- pnadc_ocup %>% mutate(outlier = rendef > 7000)

# Box-plot (rendimento)
pnadc_ocup %>%
    filter(!outlier) %>%
    ggplot(aes(x=sexo, y=rendef, fill=sexo)) + 
    geom_boxplot() +
    coord_flip()

# Densidade do (ln(rendimento/hora))
pnadc_ocup %>%
    filter(!outlier) %>%
    ggplot(aes(x = lrendh, fill = sexo)) +
    geom_density(alpha = .3)



#########################################################

# Ajustar regressao linear simples para salario x sexo
reg1 <- lm(lrend ~ sexo, pnadc_ocup)
reg1

reg2 <- lm(lrendh ~ sexo + 0, data = pnadc_ocup)
reg2

regm1 <- lm(log1p(rendef) ~ sexo + escol, data = pnadc_ocup)
summary(regm1)

regm2 <- lm(log1p(rend) ~ sexo + escol + 0, data = pnadc_ocup)
summary(regm2)


# grafico
ggplot(pnadc_ocup, aes(x = escol, y = log1p(rend), color = sexo))+
    # Diagrama de dispers?o
    geom_point() +
    # Linha 
    geom_smooth(method = "lm", se = FALSE)


library(moderndive)
ggplot(pnadc_ocup, aes(x = escol, y = log1p(rend), color = sexo)) +
    # Diagrama de dispersao
    geom_point() +
    # Linhas de regressao
    geom_parallel_slopes(se = FALSE)
