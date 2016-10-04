A função será usada nos casos em que o vetor INDENIZAÇÃO seja da classe character.
A função abaixo cria erros quando os valores já são númericos.

limpa.num <- function(x) {
  add<- x
  add<-gsub(pattern = "R",replacement = "",x = add)
  add<-gsub(pattern = "\\$",replacement = "",x = add)
  add<-gsub(pattern = " ",replacement = "",x = add)
  add<-gsub(pattern = "\\.",replacement = "",x = add)
  add<-gsub(pattern = ",",replacement = ".",x = add)
  add<-as.numeric(add)
  return(add)
}

rotulo<-c("ANO","CODIGO_SEGURADORA","NPROP","NAPOL","EVENTO_PREPONDERANTE","INDENIZAÇÃO")

"ALIANÇA"
v1<-read.csv2("//masrv03/DGRR/SINISTROS (HISTÓRICO)/2º trabalho - 2016/ALIANÇA/Aliança_2.csv", stringsAsFactors = FALSE)
v1<-v1[,-1]
names(v1)<-rotulo
str(v1)

"ALLIANZ"
v2<-read.csv2("//masrv03/DGRR/SINISTROS (HISTÓRICO)/2º trabalho - 2016/ALLIANZ/Allinz_final.csv", stringsAsFactors = FALSE)
names(v2)<-rotulo
str(v2)

"EXCELSIOR"
v3<-read.csv2("//masrv03/DGRR/SINISTROS (HISTÓRICO)/2º trabalho - 2016/EXCELSIOR/Excelsior.csv", stringsAsFactors = FALSE)
names(v3)<-rotulo
str(v3)
v3$INDENIZAÇÃO<-limpa.num(v3$INDENIZAÇÃO)
str(v3)

"FAIRFAX"
v4<-read.csv2("//masrv03/DGRR/SINISTROS (HISTÓRICO)/2º trabalho - 2016/FAIRFAX/MAPA Sinistro_FAIRFAX.csv", stringsAsFactors = FALSE)
names(v4)<-rotulo
str(v4)
v4$INDENIZAÇÃO<-limpa.num(v4$INDENIZAÇÃO)
str(v4)

"MAPFRE"
v5<-read.csv2("//masrv03/DGRR/SINISTROS (HISTÓRICO)/2º trabalho - 2016/MAPFRE/MAPFRE.csv", stringsAsFactors = FALSE)
names(v5)<-rotulo
str(v5)
v5$INDENIZAÇÃO<-limpa.num(v5$INDENIZAÇÃO)
str(v5)

"NOBRE"
v6<-read.csv2("//masrv03/DGRR/SINISTROS (HISTÓRICO)/2º trabalho - 2016/NOBRE/Mapasinistro052016.csv", stringsAsFactors = FALSE)
names(v6)<-rotulo
str(v6)

"PORTO"
v7<-read.csv2("//masrv03/DGRR/SINISTROS (HISTÓRICO)/2º trabalho - 2016/PORTO/Porto_2.csv", stringsAsFactors = FALSE)
v7<-v7[,-1]
names(v7)<-rotulo
str(v7)
v7$INDENIZAÇÃO<-limpa.num(v7$INDENIZAÇÃO)
str(v7)

"SANCOR"
v8<-read.csv2("//masrv03/DGRR/SINISTROS (HISTÓRICO)/2º trabalho - 2016/SANCOR/Sancor (003).csv", stringsAsFactors = FALSE)
v8<-v8[,-5]
names(v8)<-rotulo
str(v8)
v8$INDENIZAÇÃO<-limpa.num(v8$INDENIZAÇÃO)
str(v8)

"SWISS" 
v9<-read.csv2("//masrv03/DGRR/SINISTROS (HISTÓRICO)/2º trabalho - 2016/SWISS/Swiss RE_2.csv", stringsAsFactors = FALSE)
v9<-v9[,-1]
names(v9)<-rotulo
str(v9)
v9$INDENIZAÇÃO<-limpa.num(v9$INDENIZAÇÃO)
str(v9)

"ESSOR" 
v10<-read.csv2("//masrv03/DGRR/SINISTROS (HISTÓRICO)/2º trabalho - 2016/ESSOR/Essor (1).csv", stringsAsFactors = FALSE)
names(v10)<-rotulo
str(v10)
v10$INDENIZAÇÃO<-as.numeric(v10$INDENIZAÇÃO)
str(v10)

geral<-rbind(v1,v2,v3,
             v4,v5,v6,
             v7,v8,v9,v10)
str(geral)

#eventos
eventos.foi<-unique(geral$EVENTO_PREPONDERANTE)
write.csv2(eventos.foi,"EVENTO_PREPONDERANTE.csv")
eventos.veio<-read.csv2("//masrv03/DGRR/SINISTROS (HISTÓRICO)/2º trabalho - 2016/EVENTO_PREPONDERANTE.csv", stringsAsFactors = FALSE)
rm(eventos.foi)

Juntando:
eventos.veio<-eventos.veio[,-1]
names(eventos.veio)<-c("EVENTO_PREPONDERANTE","EVENTO_PREP_AGR")
names(geral)

geral<-merge(geral,eventos.veio,all=F)
geral<-geral[,-1]
names(geral)[6]<-rotulo[5]
write.csv2(geral,"SINISTROS_PSR.csv")




#tabelas de comparação visual

vt<-limpa.num(v10$INDENIZAÇÃO)
teste<-as.data.frame(cbind(um=v10$INDENIZAÇÃO,dois=vt))
teste<-teste[order(teste$um,decreasing = T),]

teste<-
  tapply(v10$INDENIZAÇÃO,v10$ANO,FUN= function (x) {sum(x,na.rm =T)})
write.csv2(teste,"teste.csv")
rm(vt,teste)


teste<-tapply(geral$INDENIZAÇÃO,geral$CODIGO_SEGURADORA,FUN= function (x) {sum(x,na.rm =T)})


EV<-c(
"SEM EVENTOS",
"SECA",
"GRANIZO",
"VARIAÇÃO EXCESSIVA DE TEMPERATURA",
"VENTOS FORTES",
"CHUVA EXCESSIVA",
"TROMBA D´ÁGUA",
"GEADA",
"RAIO",
"VENTOS FRIOS",
"INCÊNDIO",
"VENTOS FORTES",
"MORTE",
"OUTROS",
"ALAGAMENTO",
"REDUÇÃO DE FATURAMENTO",
"ALAGAMENTO",
"OUTROS",
"OUTROS",
"OUTROS",
"MORTE",
"MORTE",
"OUTROS",
"OUTROS",
"CHUVA EXCESSIVA",
"VARIAÇÃO EXCESSIVA DE TEMPERATURA",
"INCÊNDIO",
"OUTROS",
"OUTROS",
"ALAGAMENTO",
"CHUVA EXCESSIVA",
"SECA",
"TROMBA D´ÁGUA",
"SEM EVENTOS",
"SECA",
"TROMBA D´ÁGUA",
"TROMBA D´ÁGUA",
"GEADA",
"GRANIZO",
"OUTROS",
"INCÊNDIO",
"VENTOS FORTES",
"ALAGAMENTO",
"VENTOS FRIOS",
"QUALIDADE",
"QUEDA DE PARREIRAL",
"MORTE",
"GRANIZO",
"CHUVA EXCESSIVA",
"SECA",
"GEADA",
"VENTOS FORTES",
"ALAGAMENTO",
"INCÊNDIO",
"TROMBA D´ÁGUA",
"GRANIZO",
"GRANIZO",
"SEM EVENTOS",
"GEADA",
"CHUVA EXCESSIVA",
"TROMBA D´ÁGUA",
"VENTOS FORTES",
"GEADA",
"VARIAÇÃO EXCESSIVA DE TEMPERATURA",
"MORTE",
"MORTE",
"MORTE",
"MORTE",
"VENTOS FORTES",
"INCÊNDIO",
"MORTE",
"OUTROS",
"OUTROS",
"TROMBA D´ÁGUA",
"VARIAÇÃO EXCESSIVA DE TEMPERATURA",
"GRANIZO")

unique(EV)


