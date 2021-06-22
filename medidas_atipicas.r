# Artigo - Medidas executivas atípicas
#Autoria:  Marcello Filgueiras e Gabriel Ribeiro Brega


library(tidyverse)
library(tjsp)




# Baixando Jurisprudência -------------------------------------------------



tjsp::baixar_cjsg(livre= "medida executiva atipica",
                  diretorio= "data_raw/jurisprudencia" )

medida_executiva_atipica_raw <- tjsp::ler_cjsg(diretorio= "data_raw/jurisprudencia")




# Filtros Iniciais --------------------------------------------------------

#Filtros Formais

#Estamos Buscando por processos de Execução Civil ou Fiscal que contenha medidas atípicas.
#Por isso, tudo que for julgado em Câmaras Criminais não é de nosso interesse.
# Esse Filtro poderia ter sido feito até mesmo antes, na hora de baixar, mas o Nº de julgados
# não era o bastante grande. Por isso filtrei aqui as câmaras criminais.

medida_executiva_atipica_filtro_formal<- medida_executiva_atipica_raw %>%
  mutate(across(.cols=classe:orgao_julgador,str_to_lower),
         ano= lubridate::year(data_julgamento)) %>%
  filter(!str_detect(orgao_julgador, "criminal"))%>%
  filter(!str_detect(classe,paste("representação",
                                  "recurso em sentido estrito",
                                  "reclamação",
                                  "remessa necessária criminal",
                                  "revisão criminal",
                                  "queixa crime",
                                  "cautelar",
                                  "mandado de injunção",
                                  "dissídio coletivo de greve",
                                  "constitucionalidade",
                                  "conflito de competência",
                                  "apelação criminal",
                                  "agravo em execução penal",
                                  "ação penal",
                                  "ação rescisória",
                                  "inquérito policial",
                                  sep = "|")))


table(medida_executiva_atipica_filtro_formal$classe)

medida_executiva_atipica_filtro_formal%>%
  group_by(classe)%>%
  count(ano) %>%
  ggplot(aes(x=ano, y= n, fill= classe)) +geom_col() +
  labs(title = "Acórdãos do TJSP que mencionam Medidas Executivas Atipicas por Ano",
       subtitle =  "Filtrados Acórdãos Criminais, ADIs, Mandado de Injunção e Rescisória",
       caption = "Fonte: TJSP - Elaboração: Eu e Bregão" ) +
  theme_minimal()
