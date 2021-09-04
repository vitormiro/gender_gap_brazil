
## Analise do diferencial de rendimentos do trabalho por genero
## Autores: Vitor Hugo Miro e Natalia Carvalho

## Análise de dados da PNAD-Covid19

###--------------------------------------------------------------------------###
options(scipen = 999)

# Caso precise instalar retire o '#'
# install.packages('COVIDIBGE')

library(COVIDIBGE)
library(tidyverse)
library(survey)
library(srvyr)
library(skimr)
library(knitr)
library(kableExtra)


pcovid5 <- get_covid(year = 2020,
                   month = 5,
                   labels = FALSE,
                   deflator = TRUE,
                   design = TRUE)

# Object Class
class(pcovid5)

# Structure
glimpse(pcovid5)

###--------------------------------------------------------------------------###
# srvyr brings parts of dplyr's syntax to survey analysis, 
# using the survey package.
library(srvyr)

covid5.svy <- as_survey(pcovid5)

class(covid5.svy)

glimpse(covid5.svy)

###--------------------------------------------------------------------------###

svytable(~sexo, design = covid5.svy)

covid5.svy <- covid5.svy %>%
    mutate(sexo = factor(ifelse(A003 == 1, "Homem", "Mulher")),
           
           cor = factor(case_when(A004==1 | A004==3 ~ "branco",
                                  A004==2 | A004==4 | A004==5 ~ "preto ou pardo")),
           
           gidade = factor(case_when(A002 %in% 14:24 ~ "14-24", 
                                     A002 %in% 25:34 ~ "25-34",
                                     A002 %in% 35:49 ~ "35-49", 
                                     A002 %in% 50:64 ~ "50-64", 
                                     A002 > 64 ~ "65+")),
           
           educ = factor(case_when(A005==1 ~ "sem instrucao",
                                   A005==2 ~ "fundamental incomp",
                                   A005==3 ~ "fundamental comp",
                                   A005==4 ~ "medio incomp",
                                   A005==5 ~ "medio comp",
                                   A005==6 ~ "superior incomp",
                                   A005==7 ~ "superior comp",
                                   A005==8 ~ "pos graduacao ou mais")),
           
           regiao = factor(case_when(UF %in% 11:17 ~ "Norte", 
                                     UF %in% 21:29 ~ "Nordeste", 
                                     UF %in% 31:35 ~ "Sudeste", 
                                     UF %in% 41:43 ~ "Sul",
                                     UF %in% 50:53 ~ "Centro Oeste")),
           
           ocupado = factor(ifelse((C001==1 | C002==1) & C007!=9, "sim", "nao")),
           
           ocup_afastado = factor(ifelse(ocupado=="sim" & C002==1, "sim", "nao")),
           
           ocup_naoafastado = factor(ifelse((C001==1 | C002==2) & C007!=9, "sim", "nao")),
           
           home_office = factor(ifelse(C013 == 1, "sim", "nao")),
           
           informal = factor(ifelse((C007==1 |C007==4 ) & C007B==3 |
                                 (C007==6 |C007==7 ) & C014==2 |
                                 C007==8 & ocupado==1, "Informal", "Formal")),
           
           cat_mt = factor(case_when(C007==01 ~ "trabalhador domestico",
                                     C007==02 ~ "militar",
                                     C007==03 ~ "policial ou bombeiro militar",
                                     C007==04 ~ "empregado setor privado",
                                     C007==05 ~ "empregado setor p?blico",
                                     C007==06 ~ "empregador",
                                     C007==07 ~ "contra propria",
                                     C007==08 ~ "trabalhador familiar nao remun",
                                     C007==09 ~ "fora do mercado de trabalho")))
           

###############################
# RENDIMENTO DO TRABALHO 
covid5.svy <- covid5.svy %>% mutate(
    # Renda do trabalho habitual (recebidas em dinheiro ou em mercadorias)
    rt1_hab = ifelse(is.na(C01012), 0, C01012),
    rt2_hab = ifelse(is.na(C01022), 0, C01022),
    # Renda do trabalho efetiva (recebidas em dinheiro ou em mercadorias)
    rt1_ef = ifelse(is.na(C011A12), 0, C011A12),
    rt2_ef = ifelse(is.na(C011A22), 0, C011A22))

## Agregar rendimentos
covid5.svy <- covid5.svy %>% mutate(
    rt_hab = ifelse(is.na(C01012) & is.na(C01022), NA, rt1_hab+rt2_hab),
    rt_ef = ifelse(is.na(C011A12) & is.na(C011A22), NA, rt1_ef+rt2_ef))

# Medias de rendimentos do trabalho
svymean(~rt_hab + rt_ef, covid5.svy, na.rm = T)


# Media de rendimentos do trabalho HABITUAL por sexo
svyby(formula = ~rt_hab, 
      by = ~sexo, 
      design = covid5.svy, 
      FUN = svymean, 
      na.rm = TRUE, 
      row.names = FALSE)

# Media de rendimentos do trabalho EFETIVO por sexo
svyby(formula = ~rt_ef, 
      by = ~sexo, 
      design = covid5.svy, 
      FUN = svymean, 
      na.rm = TRUE, 
      row.names = FALSE)



###--------------------------------------------------------------------------###
### Estimativa de rendimentos do trabalho (média e IC) por sexo

rendtrab_sexo <-covid5.svy %>% 
    group_by(sexo) %>% 
    summarise(rendtrab_hab = survey_mean(rt_hab,
                                           vartype = c("ci"),
                                           level = 0.95,
                                           na.rm = TRUE),
              rendtrab_ef = survey_mean(rt_ef, 
                                          vartype = c("ci"),
                                          level = 0.95,
                                          na.rm = TRUE))

###--------------------------------------------------------------------------###
