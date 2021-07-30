
## Analise do diferencial de rendimentos do trabalho por genero
## Autores: Vitor Hugo Miro e Natalia Carvalho

###--------------------------------------------------------------------------###
# limpar "Area de trabalho" do R
rm(list = ls())
options(scipen = 999) # para evitar notacao cientifica.

###--------------------------------------------------------------------------###

# Pacotes
library(PNADcIBGE)
library(survey)

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

# Carrega dados da PNADC 
pnadc <- get_pnadc(year = 2019,
                   vars = variaveis,
                   quarter = 4,
                   design = TRUE,
                   labels = FALSE,
                   deflator = TRUE,
                   defyear = 2019)

# Classe do objeto gerado
class(pnadc)  

# estrutura dos dados
glimpse(pnadc)


saveRDS(pnadc, "svypnadc19.rds")


###--------------------------------------------------------------------------###

# Pacotes
library(tidyverse)
library(srvyr)

library(knitr)
library(kableExtra)

# Aplica funcao 'as_survey' do pacote srvyr
pnadc <- as_survey(pnadc)

class(pnadc)

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
                                   VD3004==7 ~ "superior comp")),
           
           regiao = factor(case_when(UF %in% 10:19 ~ "Norte",
                                     UF %in% 20:29 ~ "Nordeste",
                                     UF %in% 30:39 ~ "Sudeste",
                                     UF %in% 40:49 ~ "Sul",
                                     UF %in% 50:59 ~ "Centro Oeste")))



# Amostra
svytotal(~sexo, pnadc, na.rm = T)
svytotal(~sexo, subset(pnadc, V2009 >=25), na.rm = T) 

# Proporções
svymean(~sexo, pnadc, na.rm = T)
svymean(~sexo, subset(pnadc, V2009 >=25), na.rm = T)


# Manter na base apenas indivíduos com idade de 25 anos ou mais
pnadc <- pnadc %>% filter(V2009 >=25)

##################################
# Tabela de frequencia por sexo
tab1 <- svytable(~sexo, design = pnadc)
tab1

# Salvando como df e adicionando proporcoes
tab1 <- tab1 %>%
    as.data.frame() %>%
    mutate(Prop = Freq/sum(Freq))
tab1

# Grafico (barras)
ggplot(data = tab1,
       mapping = aes(x = sexo, y = Prop)) + 
    geom_col() +
    coord_flip()


###################################
# Tabela de frequencia por ocupacao

# Tabela de frequencia por sexo
tab2 <- svytable(~ocup, design = pnadc)
tab2

# Salvando como df e adicionando proporcoes
tab2 <- tab2 %>%
    as.data.frame() %>%
    mutate(Prop = Freq/sum(Freq))
tab2

# Grafico (barras)
ggplot(data = tab2,
       mapping = aes(x = ocup, y = Prop)) + 
    geom_col() +
    coord_flip()


########################################
# Visualizar ocupação e sexo
tab3 <- svytable(~sexo + ocup, design = pnadc)
tab3

# Adicionar proporcoes condicionais
tab3 <- tab3 %>%
    as.data.frame() %>%
    group_by(sexo) %>%
    mutate(n_sexo = sum(Freq), Prop_ocup = Freq/sum(Freq)) %>%
    ungroup()
tab3

# Grafico (barras)
ggplot(data = tab3,
       mapping = aes(x = sexo, y = Prop_ocup, fill = ocup)) + 
    geom_col() + 
    coord_flip()



########################################
# Visualizar escolaridade e sexo
tab4 <- svytable(~sexo + educ, design = pnadc)
tab4

# Adicionar proporcoes condicionais
tab4 <- tab4 %>%
    as.data.frame() %>%
    group_by(sexo) %>%
    mutate(Prop_educ = Freq/sum(Freq)) %>%
    ungroup()
tab4

# Grafico (barras)
ggplot(data = tab4,
       mapping = aes(x = sexo, y = Prop_educ, fill = educ)) + 
    geom_col() + 
    coord_flip()

# inverter
# Adicionar proporcoes condicionais
tab5 <- tab4 %>%
    as.data.frame() %>%
    group_by(educ) %>%
    mutate(Prop_sexo = Freq/sum(Freq)) %>%
    ungroup()
tab5

# Grafico (barras)
ggplot(data = tab5,
       mapping = aes(x = educ, y = Prop_sexo, fill = sexo)) + 
    geom_col() + 
    coord_flip()


##################################################################

# Filtrar apenas ocupados
pnadc_ocup <- filter(pnadc, ocup == "ocupado")


# Salários
svymean(x = ~VD4020, design = pnadc_ocup, na.rm = TRUE)


# Salários x sexo
svyby(formula = ~VD4020, by = ~sexo, 
      design = pnadc_ocup, 
      FUN = svymean, na.rm = TRUE, 
      row.names = FALSE)


# Quantis dos salarios
svyquantile(x = ~VD4020, 
            design = pnadc_ocup, 
            na.rm = TRUE, 
            quantiles = c(0.1, 0.25, 0.5, 0.75, .90))


# Quantis dos salarios x sexo
svyby(formula = ~VD4020, 
      by = ~sexo, 
      design = pnadc_ocup, 
      FUN = svyquantile, 
      na.rm = TRUE, 
      quantiles = c(0.1, 0.25, 0.5, 0.75, .90), 
      keep.rows = FALSE, 
      keep.var = FALSE)

