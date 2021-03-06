A fun��o ser� usada nos casos em que o vetor INDENIZA��O seja da classe character.
A fun��o abaixo cria erros quando os valores j� s�o n�mericos.

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

rotulo<-c("ANO","CODIGO_SEGURADORA","NPROP","NAPOL","EVENTO_PREPONDERANTE","INDENIZA��O")

"ALIAN�A"
v1<-read.csv2("//masrv03/DGRR/SINISTROS (HIST�RICO)/2� trabalho - 2016/ALIAN�A/Alian�a_2.csv", stringsAsFactors = FALSE)
v1<-v1[,-1]
names(v1)<-rotulo
str(v1)

"ALLIANZ"
v2<-read.csv2("//masrv03/DGRR/SINISTROS (HIST�RICO)/2� trabalho - 2016/ALLIANZ/Allinz_final.csv", stringsAsFactors = FALSE)
names(v2)<-rotulo
str(v2)

"EXCELSIOR"
v3<-read.csv2("//masrv03/DGRR/SINISTROS (HIST�RICO)/2� trabalho - 2016/EXCELSIOR/Excelsior.csv", stringsAsFactors = FALSE)
names(v3)<-rotulo
str(v3)
v3$INDENIZA��O<-limpa.num(v3$INDENIZA��O)
str(v3)

"FAIRFAX"
v4<-read.csv2("//masrv03/DGRR/SINISTROS (HIST�RICO)/2� trabalho - 2016/FAIRFAX/MAPA Sinistro_FAIRFAX.csv", stringsAsFactors = FALSE)
names(v4)<-rotulo
str(v4)
v4$INDENIZA��O<-limpa.num(v4$INDENIZA��O)
str(v4)

"MAPFRE"
v5<-read.csv2("//masrv03/DGRR/SINISTROS (HIST�RICO)/2� trabalho - 2016/MAPFRE/MAPFRE.csv", stringsAsFactors = FALSE)
names(v5)<-rotulo
str(v5)
v5$INDENIZA��O<-limpa.num(v5$INDENIZA��O)
str(v5)

"NOBRE"
v6<-read.csv2("//masrv03/DGRR/SINISTROS (HIST�RICO)/2� trabalho - 2016/NOBRE/Mapasinistro052016.csv", stringsAsFactors = FALSE)
names(v6)<-rotulo
str(v6)

"PORTO"
v7<-read.csv2("//masrv03/DGRR/SINISTROS (HIST�RICO)/2� trabalho - 2016/PORTO/Porto_2.csv", stringsAsFactors = FALSE)
v7<-v7[,-1]
names(v7)<-rotulo
str(v7)
v7$INDENIZA��O<-limpa.num(v7$INDENIZA��O)
str(v7)

"SANCOR"
v8<-read.csv2("//masrv03/DGRR/SINISTROS (HIST�RICO)/2� trabalho - 2016/SANCOR/Sancor (003).csv", stringsAsFactors = FALSE)
v8<-v8[,-5]
names(v8)<-rotulo
str(v8)
v8$INDENIZA��O<-limpa.num(v8$INDENIZA��O)
str(v8)

"SWISS" 
v9<-read.csv2("//masrv03/DGRR/SINISTROS (HIST�RICO)/2� trabalho - 2016/SWISS/Swiss RE_2.csv", stringsAsFactors = FALSE)
v9<-v9[,-1]
names(v9)<-rotulo
str(v9)
v9$INDENIZA��O<-limpa.num(v9$INDENIZA��O)
str(v9)

"ESSOR" 
v10<-read.csv2("//masrv03/DGRR/SINISTROS (HIST�RICO)/2� trabalho - 2016/ESSOR/Essor (1).csv", stringsAsFactors = FALSE)
names(v10)<-rotulo
str(v10)
v10$INDENIZA��O<-as.numeric(v10$INDENIZA��O)
str(v10)

geral<-rbind(v1,v2,v3,
             v4,v5,v6,
             v7,v8,v9,v10)
str(geral)

#eventos
eventos.foi<-unique(geral$EVENTO_PREPONDERANTE)
write.csv2(eventos.foi,"EVENTO_PREPONDERANTE.csv")
eventos.veio<-read.csv2("//masrv03/DGRR/SINISTROS (HIST�RICO)/2� trabalho - 2016/EVENTO_PREPONDERANTE.csv", stringsAsFactors = FALSE)
rm(eventos.foi)

Juntando:
eventos.veio<-eventos.veio[,-1]
names(eventos.veio)<-c("EVENTO_PREPONDERANTE","EVENTO_PREP_AGR")
names(geral)

geral<-merge(geral,eventos.veio,all=F)
geral<-geral[,-1]
names(geral)[6]<-rotulo[5]
write.csv2(geral,"SINISTROS_PSR.csv")




#tabelas de compara��o visual

vt<-limpa.num(v10$INDENIZA��O)
teste<-as.data.frame(cbind(um=v10$INDENIZA��O,dois=vt))
teste<-teste[order(teste$um,decreasing = T),]

teste<-
  tapply(v10$INDENIZA��O,v10$ANO,FUN= function (x) {sum(x,na.rm =T)})
write.csv2(teste,"teste.csv")
rm(vt,teste)


teste<-tapply(geral$INDENIZA��O,geral$CODIGO_SEGURADORA,FUN= function (x) {sum(x,na.rm =T)})


EV<-c(
"SEM EVENTOS",
"SECA",
"GRANIZO",
"VARIA��O EXCESSIVA DE TEMPERATURA",
"VENTOS FORTES",
"CHUVA EXCESSIVA",
"TROMBA D��GUA",
"GEADA",
"RAIO",
"VENTOS FRIOS",
"INC�NDIO",
"VENTOS FORTES",
"MORTE",
"OUTROS",
"ALAGAMENTO",
"REDU��O DE FATURAMENTO",
"ALAGAMENTO",
"OUTROS",
"OUTROS",
"OUTROS",
"MORTE",
"MORTE",
"OUTROS",
"OUTROS",
"CHUVA EXCESSIVA",
"VARIA��O EXCESSIVA DE TEMPERATURA",
"INC�NDIO",
"OUTROS",
"OUTROS",
"ALAGAMENTO",
"CHUVA EXCESSIVA",
"SECA",
"TROMBA D��GUA",
"SEM EVENTOS",
"SECA",
"TROMBA D��GUA",
"TROMBA D��GUA",
"GEADA",
"GRANIZO",
"OUTROS",
"INC�NDIO",
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
"INC�NDIO",
"TROMBA D��GUA",
"GRANIZO",
"GRANIZO",
"SEM EVENTOS",
"GEADA",
"CHUVA EXCESSIVA",
"TROMBA D��GUA",
"VENTOS FORTES",
"GEADA",
"VARIA��O EXCESSIVA DE TEMPERATURA",
"MORTE",
"MORTE",
"MORTE",
"MORTE",
"VENTOS FORTES",
"INC�NDIO",
"MORTE",
"OUTROS",
"OUTROS",
"TROMBA D��GUA",
"VARIA��O EXCESSIVA DE TEMPERATURA",
"GRANIZO")

unique(EV)


