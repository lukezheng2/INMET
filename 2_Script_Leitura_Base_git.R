library(rvest)
library(tidyverse)
library(httr)
library(XML)

# Lista de estações do INMET
estacoes <- data.table::fread("Estacoes")
#lista_estacoes<-read.csv("estacoes.csv", header = TRUE, sep=";", skip=1)[,3]

# Função que extrai os dados do INMET a partir de um link
# O link deve ser do site do INMET
ler_inmet<-function(Site,Tratado=T){
  url <-"http://www.inmet.gov.br/projetos/rede/pesquisa/inicio.php"
  # Iniciando a sessão
  pgsession <- html_session(url) 
  # Lista com os espaços para preenchimento
  pgform <- html_form(pgsession)[[1]]
  # Completar de acordo com o pgform
  filled_form <- set_values(pgform,
                            "mCod" = "a",
                            "mSenha" = "b")
  # Apertar o botão "entrar"
  teste <- submit_form(pgsession,filled_form,'btnProcesso')
  memberlist <- jump_to(pgsession, paste0(site))
  # Extraindo informações da página
  page <- read_html(memberlist)
  dadosPag <- as.character(html_text(html_node(page,'pre')))
  # A página retorna um txt em 4 partes, sendo:
  # Primeira e Terceira: nada de importante
  # Segunda: Estação, Latitude e Longitude
  # Quarta: Dados temporais
  dadosPag2 <- strsplit(dadosPag, "\n")
  dadosPag3 <- strsplit(dadosPag,"\n--------------------\n")
  # Extraindo e limpando os dados
  dados <- read.table(text = dadosPag3[[1]][4], sep=";", header = TRUE) 
  dados12 <- dados %>% filter(Hora==1200) %>% select(-Hora)
  dados0 <- dados %>% filter(Hora==0) %>% select(-Hora)
  
  
  #Incluindo Estação, Latitude e Longitude
  estacao <- strsplit(dadosPag2[[1]][4], ": |\\(")[[1]][2]
  lat <- strsplit(dadosPag2[[1]][5], ": ")[[1]][2]
  long <- strsplit(dadosPag2[[1]][6], ": ")[[1]][2]
  alt <- strsplit(dadosPag2[[1]][7], ": ")[[1]][2]
  dados <- cbind(dados,estacao,lat,long,alt)
  gc()
  return(dados)
}

site<-("http://www.inmet.gov.br/projetos/rede/pesquisa/gera_serie_txt.php?&mRelEstacao=82989&btnProcesso=serie&mRelDtInicio=01/01/1961&mRelDtFim=01/01/2016&mAtributos=,,1,1,,,,,,1,1,,1,1,1,1,")
dados<-ler_inmet(site)
