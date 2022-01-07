
## Analise do diferencial de rendimentos do trabalho por genero
## Análise exploratória
## Autores: Vitor Hugo Miro e Natalia Carvalho

###--------------------------------------------------------------------------###
# limpar "Area de trabalho" do R
rm(list = ls())
options(scipen = 999) # para evitar notacao cientifica.

###--------------------------------------------------------------------------###

# Pacotes
library(PNADcIBGE)
library(tidyverse)
library(tidymodels)
library(fastDummies)
library(survey)
library(srvyr)
library(knitr)
library(kableExtra)

###--------------------------------------------------------------------------###
# Definir variaveis utilizadas na analise
variaveis <- c('Ano', 'Trimestre', 'UF', 'UPA', 'Estrato', 
               'V1008', 'V1014', 'V1027', 'V1028',
               'V2007', # sexo
               'V2009', # idade
               'V2010', # cor/raca
               'VD3004', # Escolaridade
               'VD3005', # Anos de estudo
               'VD4001', # Condicao em relacao a forca de trabalho
               'VD4002', # Condicao de ocupacao
               'VD4009', # Cond ocup e cat emprego
               'VD4019', # Rend. todos trabalhos habitual
               'VD4020', # Rend. todos trabalhos efetivo
               'VD4031', # Horas habitual trabalhadas em todos os trabalhos
               'VD4035'  # Horas efetivas trabalhadas em todos os trabalhos
)


###--------------------------------------------------------------------------###
###--------------------------------------------------------------------------###
## ESSA PARTE DO CÓDIGO SÓ É EXECUTADA NA PRIMEIRA VEZ, PARA BAIXAR OS DADOS
# Carrega dados da PNADC 
pnadc <- get_pnadc(year = 2019,
                   vars = variaveis,
                   quarter = 4,
                   design = FALSE,
                   labels = FALSE,
                   deflator = FALSE,
                   defyear = 2019)

# Classe do objeto gerado
class(pnadc)  

# estrutura dos dados
glimpse(pnadc)

saveRDS(pnadc, "pnadc19.rds")

###--------------------------------------------------------------------------###
###--------------------------------------------------------------------------###
# ler dados salvos (após os comandos acima podemos iniciar)
pnadc <- readRDS("pnadc19.rds")

# Classe do objeto gerado
class(pnadc)  

# estrutura dos dados
glimpse(pnadc)


###--------------------------------------------------------------------------###
### ANALISE EXPLORATORIA INICIAL

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


###--------------------------------------------------------------------------###

# Aplicar o desenho amostral da PNADC
svypnadc <- pnadc_design(pnadc)
class(svypnadc )

# Aplica funcao 'as_survey' do pacote srvyr. Para uso de tidyverse.
svypnadc <- as_survey(svypnadc)
class(svypnadc )


###--------------------------------------------------------------------------###
### ANALISE EXPLORATORIA INICIAL

# Amostra: svytotal(~sexo, pnadc, na.rm = T)
# proporção: svymean(~sexo, pnadc, na.rm = T)

# amostra e proporção
svypnadc %>% 
    group_by(sexo) %>% 
    summarise(total = survey_total(),
              prop = round(100*survey_mean(), 2))


svytable(~eativo, design = svypnadc)
svytable(~ocup, design = svypnadc)


###------------------------------------------------------------------------###
# FILTRO: Manter na base apenas indivíduos com idade de 25 anos ou mais
svypnadc <- svypnadc %>% filter(idade >=25)

# total
svypnadc %>% 
    group_by(sexo) %>% 
    summarise(total = survey_total(),
              prop = round(100*survey_mean()))

# Ativos = na força de trabalho
svypnadc %>% 
    group_by(sexo) %>% 
    summarise(ativos = survey_total(eativo == "econ.ativo", na.rm = TRUE),
              p_ativos = round(100*survey_mean(eativo == "econ.ativo", 
                                               na.rm = TRUE), 2))

# Ocupados
svypnadc %>% 
    group_by(sexo) %>% 
    summarise(ocupados = survey_total(ocup == "ocupado", na.rm = TRUE),
              p_ocupados = round(100*survey_mean(ocup == "ocupado", 
                                             na.rm = TRUE), 2))


###------------------------------------------------------------------------###
# FILTRO: Manter na base apenas indivíduos ocupados com rendimentos positivos
svypnadc <- svypnadc %>% filter(ocup == "ocupado" & rendef > 0)

# Rendimentos do trabalho (efetivo)
svypnadc %>% 
    group_by(sexo) %>% 
    summarise(rend_medio = survey_mean(rendef, na.rm = TRUE),
              rend_hora_medio = survey_mean(rendefh, na.rm = TRUE))


###------------------------------------------------------------------------###



