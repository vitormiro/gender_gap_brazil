
## Analise do diferencial de rendimentos do trabalho por genero
## Modelagem
## Autores: Vitor Hugo Miro e Natalia Carvalho

###--------------------------------------------------------------------------###
# limpar "Area de trabalho" do R
rm(list = ls())
options(scipen = 999) # para evitar notacao cientifica.

###--------------------------------------------------------------------------###

# Pacotes
library(tidyverse)
library(tidymodels)
library(fastDummies)
library(knitr)
library(kableExtra)

###--------------------------------------------------------------------------###

# ler dados salvos (após os comandos acima podemos iniciar)
pnadc <- readRDS("pnadc19.rds")

# Classe do objeto gerado
class(pnadc)  

# estrutura dos dados
glimpse(pnadc)


###--------------------------------------------------------------------------###
### Ajustar variáveis

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
                                   VD3004==7 ~ "superior comp"),
                         levels = c("sem instrução", "fundamental incomp", 
                                    "fundamental comp", "médio incomp", "médio comp",
                                    "superior incomp", "superior comp")),
           
           regiao = factor(case_when(UF %in% 10:19 ~ "Norte",
                                     UF %in% 20:29 ~ "Nordeste",
                                     UF %in% 30:39 ~ "Sudeste",
                                     UF %in% 40:49 ~ "Sul",
                                     UF %in% 50:59 ~ "Centro Oeste")))

pnadc <- pnadc %>% rename(idade = V2009,
                          anos_estudo = VD3005,
                          rendef = VD4020,
                          horas = VD4035)

pnadc <- pnadc %>% mutate(rendefh = ifelse(rendef==0 | horas == 0, NA, 
                                           rendef/horas),
                          lrendef = ifelse(rendef==0, NA, log(rendef)),
                          lrendefh = ifelse(rendefh==0, NA, log(rendefh)))


###############################################################################
## ESTIMACOES
###############################################################################
## organizar variaveis para as estimações

colnames(pnadc)

## DataFrame apenas com variáveis usadas no modelo
df1 <- pnadc %>% 
    select(Ano, Trimestre, UF, idade, educ, regiao, sexo, cor, ocup, ocup_cat, 
           rendef, lrendefh) %>% 
    filter(idade >=25 & ocup == "ocupado" & rendef > 0)


## Criar dummies
df1d <- dummy_columns(.data = df1,
                      select_columns = c("educ","regiao", "sexo", "cor"),
                      remove_selected_columns = T,
                      remove_most_frequent_dummy = T)

## versão do DataFrame para estimação
df1d <- df1d %>% 
    select(lrendefh, idade, starts_with('educ'), 
           starts_with('regiao'), 
           starts_with('sexo'), 
           starts_with('cor'))

## Estimação
lm_spec <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine("lm")

lm_fit <- lm_spec %>%
    fit(lrendefh ~ . , data = df1d)


## Resultados
tidy(lm_fit)
glance(lm_fit)
