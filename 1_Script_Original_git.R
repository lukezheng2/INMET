library(rvest)
library(tidyverse)
library(httr)
library(XML)

estacoes <- data.table::fread("Estacoes")
#lista_estacoes<-read.csv("estacoes.csv", header = TRUE, sep=";", skip=1)[,3]


ler_inmet<-function(site){
  url <-"http://www.inmet.gov.br/projetos/rede/pesquisa/inicio.php"
  pgsession <- html_session(url)
  #Lista com os espaços para preenchimento
  pgform <- html_form(pgsession)[[1]]
  #Completar de acordo com o pgform
  filled_form <- set_values(pgform,
                            "mCod" = "a",
                            "mSenha" = "b")
  #Apertar o botão "entrar"
  teste <- submit_form(pgsession,filled_form,'btnProcesso')
  memberlist <- jump_to(pgsession, paste0(site))
  page <- read_html(memberlist)
  dadosPag <- as.character(html_text(html_node(page,'pre')))
  dadosPag2 <- strsplit(dadosPag, "\n")
  dadosPag3 <- strsplit(dadosPag,"\n--------------------\n")
  dados <- read.table(text = dadosPag3[[1]][4], sep=";", header = TRUE)
  estacao <- strsplit(dadosPag2[[1]][4], ": |\\(")[[1]][2]
  lat <- strsplit(dadosPag2[[1]][5], ": ")[[1]][2]
  long <- strsplit(dadosPag2[[1]][6], ": ")[[1]][2]
  alt <- strsplit(dadosPag2[[1]][7], ": ")[[1]][2]
  dados <- cbind(dados,estacao,lat,long,alt)
  gc()
  return(dados)
}
ok<-c()
site<-("http://www.inmet.gov.br/projetos/rede/pesquisa/gera_serie_txt.php?&mRelEstacao=82989&btnProcesso=serie&mRelDtInicio=01/01/1961&mRelDtFim=01/01/2016&mAtributos=,,1,1,,,,,,1,1,,1,1,1,1,")
dados<-ler_inmet(site)



for(i in lista_estacoes){
  tryCatch({
    site<-
      paste0("http://www.inmet.gov.br/projetos/rede/pesquisa/gera_serie_txt.php?&mRelEstacao=",i,"&btn
Processo=serie&mRelDtInicio=01/01/1961&mRelDtFim=01/01/2016&mAtributos=,,1,1,,,,,,1,1,,1,1,1,1,")
    table<-ler_inmet(site)
    dados<-rbind(dados,table)
    print(paste(i, "<-"))
    ok<-rbind(ok,i)
  },error=function(e) print(i))}
library(readr)
library(data.table)
library(magrittr)
library(reshape2)
library(sp)
library(gstat)
library(readr)
library(dplyr)
data <- fread("dados.csv")
trat.dados <-function(data){
  data0 <- subset(data, data$Hora==0)
  data12 <- subset(data, data$Hora==1200)
  data_final <- merge(data0,data12, by=c("Data","estacao"), all.x=TRUE)
  data_final<-data_final[,-c("long.y","alt.y","lat.y","Precipitacao.x", "TempMinima.x", "X.x","X.y",
                             "TempMaxima.y", "Insolacao.y", "Evaporacao.Piche.y", "Temp.Comp.Media.y",
                             "Umidade.Relativa.Media.y", "Velocidade.do.Vento.Media.y")]
  data_final <- cbind(data_final,data_final$Data %>% colsplit("/", names = c("Dia", "Mes", "Ano")))
  return(data_final)
}
data_final <- trat.dados(data)
#Temperatura Máxima
interpolacao<-function(data_final, ano, mes){
  

  
  knowndt = data_final %>% dplyr::select(long.x, lat.x, TempMaxima.x, Mes, Ano)
  knowndt$lat.x<-as.numeric(knowndt$lat.x)
  knowndt$long.x<-as.numeric(knowndt$long.x)
  knowndt2 <- subset(knowndt, !is.na(TempMaxima.x))
  knowndt2 <- knowndt2[knowndt2$Ano==ano & knowndt2$Mes==mes]
  if( dim(knowndt2)[1] == 0){return(knowndt2 %>% as.data.frame)}
  sp::coordinates(knowndt2) <- ~ lat.x + long.x
  unknowndt <- fread("Municipios_Brasileiros.csv", sep=";")
  coordinates(unknowndt) <- ~ Latitude + Longitude
  
  idwmodel = idw(TempMaxima.x ~1, knowndt2,unknowndt,
                 maxdist = Inf, idp = 2)
  predZ = idwmodel@data$var1.pred
  unknowndt$Temp.Max <- predZ
  unknowndt$Ano <- ano
  unknowndt$Mes <- mes
  a <- unknowndt %>% as.data.frame
  return(a)
}
finalTempMax <- data.frame()
for(i in 1960:2017) {
  for(j in 1:12) {
    finalTempMax <- plyr::rbind.fill(finalTempMax, interpolacao(data_final, i, j))
  }
  print(i)
}

10

write.table(finalTempMax, "TempMax.txt", sep = "|")
#Temperatura Mínima
interpolacao2<-function(data_final, ano, mes){
  knowndt = data_final %>% dplyr::select(long.x, lat.x, TempMinima.y, Mes, Ano)
  knowndt$lat.x<-as.numeric(knowndt$lat.x)
  knowndt$long.x<-as.numeric(knowndt$long.x)
  knowndt2 <- subset(knowndt, !is.na(TempMinima.y))
  knowndt2 <- knowndt2[knowndt2$Ano==ano & knowndt2$Mes==mes]
  if( dim(knowndt2)[1] == 0){return(knowndt2 %>% as.data.frame)}
  sp::coordinates(knowndt2) <- ~ lat.x + long.x
  unknowndt <- data.table::fread("Municipios_Brasileiros.csv", sep=";")
  coordinates(unknowndt) <- ~ Latitude + Longitude
  
  idwmodel = idw(TempMinima.y ~1, knowndt2,unknowndt,
                 maxdist = Inf, idp = 2)
  predZ = idwmodel@data$var1.pred
  unknowndt$Temp.Min <- predZ
  unknowndt$Ano <- ano
  unknowndt$Mes <- mes
  a <- unknowndt %>% as.data.frame
  return(a)
}
finalTempMin <- data.frame()
for(i in 1960:2017) {
  for(j in 1:12) {
    
    11
    
    finalTempMin <- plyr::rbind.fill(finalTempMin, interpolacao2(data_final, i, j))
  }
  print(i)
}
write.table(finalTempMin, "TempMin.txt", sep = "|")
#Temperatura Média
interpolacao3<-function(data_final, ano, mes){
  knowndt = data_final %>% dplyr::select(long.x, lat.x, Temp.Comp.Media.x, Mes, Ano)
  knowndt$lat.x<-as.numeric(knowndt$lat.x)
  knowndt$long.x<-as.numeric(knowndt$long.x)
  knowndt2 <- subset(knowndt, !is.na(Temp.Comp.Media.x))
  knowndt2 <- knowndt2[knowndt2$Ano==ano & knowndt2$Mes==mes]
  if( dim(knowndt2)[1] == 0){return(knowndt2 %>% as.data.frame)}
  sp::coordinates(knowndt2) <- ~ lat.x + long.x
  unknowndt <- fread("Municipios_Brasileiros.csv", sep=";")
  coordinates(unknowndt) <- ~ Latitude + Longitude
  
  idwmodel = idw(Temp.Comp.Media.x ~1, knowndt2,unknowndt,
                 maxdist = Inf, idp = 2)
  predZ = idwmodel@data$var1.pred
  unknowndt$Temp.MaxCompMedia <- predZ
  unknowndt$Ano <- ano
  unknowndt$Mes <- mes
  a <- unknowndt %>% as.data.frame
  return(a)
}

12

finalTempCompMedia <- data.frame()
for(i in 1960:2017) {
  for(j in 1:12) {
    finalTempCompMedia <- plyr::rbind.fill(finalTempCompMedia, interpolacao3(data_final, i, j))
  }
  print(i)
}
write.table(finalTempCompMedia , "TempMedia.txt", sep = "|")
#Tratamento dos dados (desconsiderar)
finalTempMax <- fread("TempMax.txt",
                      fill = TRUE,
                      drop = c("n_row", "long.x", "lat.x", "TempMaxima.x"),
                      header = TRUE)
finalTempMin <- fread("TempMin.txt",
                      fill = TRUE,
                      drop = c("n_row", "long.x", "lat.x", "TempMinima.y"),
                      header = TRUE)
colnames(finalTempMin)[10] <- "Temp.min"
f1 <- inner_join(finalTempCompMedia, finalTempMax)
f1 <- inner_join(f1, finalTempMin)
colnames(f1)[10] <- "TempCompMedia"
write.table(f1, "INMET.txt", sep = ",")