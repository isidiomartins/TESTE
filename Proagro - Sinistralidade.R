# Calculo da indenização sobre a IS - LOSS COST

pasta<-"C:/Users/joao.isidio/Desktop/PROAGRO"
arq<-list.files(pasta)
arq

gerais<-read.csv2(paste0(pasta,"/",arq[3]), stringsAsFactors=F)
cop   <-read.csv2(paste0(pasta,"/",arq[2]), stringsAsFactors=F)
names(cop)[1]<-"ANO"

names(gerais)
names(cop)

rm(arq,pasta)

#Ver se os quatro elementos formam uma chave:

chave<-paste0(
gerais$ANO,
gerais$Tipo.Remessa,
gerais$cd.mun,
gerais$cd.emp)

length(unique(chave))
#A chave é válida
rm(chave)

#só pra ver como estão as COPs

chave2<-paste0(
  cop$ANO,
  cop$Tipo.Remessa,
  cop$cd.mun,
  cop$cd.emp)

length(unique(chave2))
#Essa já tem repetição
rm(chave2)

#Juntando:

base<-merge(gerais, cop, all=T,by = c("ANO","Tipo.Remessa","cd.mun","cd.emp"))
#Nesse formato não serve, duplica contratos.
rm(base)

#procurando inconsistencias:
library(dplyr)

inner<-inner_join(gerais, cop, by = c("ANO","Tipo.Remessa","cd.mun","cd.emp"))
left<-left_join(gerais, cop, by = c("ANO","Tipo.Remessa","cd.mun","cd.emp"))
right<-right_join(gerais, cop, by = c("ANO","Tipo.Remessa","cd.mun","cd.emp"))
anti<-anti_join(gerais, cop, by = c("ANO","Tipo.Remessa","cd.mun","cd.emp"))
anti2<-anti_join(cop,gerais, by = c("ANO","Tipo.Remessa","cd.mun","cd.emp"))
outer<-full_join(gerais, cop, by = c("ANO","Tipo.Remessa","cd.mun","cd.emp"))
#O anti2 diz que todas as cops estão nos contratos
#Existem casos de duas cops por contrato
rm(inner,left,right,anti,anti2,outer)

## Tratando os valores numéricos
#A função "limpa.num" fará os acertos necessários nos valores observados como numéricos.

limpa.num <- function(x) {
  add<- x
  add<-gsub(pattern = "%",replacement = "",x = add)
  add<-gsub(pattern = " ",replacement = "",x = add)
  add<-gsub(pattern = "\\.",replacement = "",x = add)
  add<-gsub(pattern = ",",replacement = ".",x = add)
  add<-as.numeric(add)
  return(add)
}


#A parte da tabelas gerais e cop com os números acertados.

gerais[, 10:11]<- as.data.frame(lapply(X = gerais[, 10:11],FUN = limpa.num))

cop[, 9:13]<- as.data.frame(lapply(X = cop[, 9:13],FUN = limpa.num))

rm(limpa.num)
#Afim de não repetir os valores de contrato quando do join, deve-se agregar os valores associados
#as cops pela chave determinada. Assim o relacionamento sera de um para um.

cop.agr<-
  cop %>%
  group_by(ANO,Tipo.Remessa,cd.mun,cd.emp) %>%
  summarise(qtcop=sum(COP...Quantidade),
            area=sum(Área.Hectares),
            vtcop=sum(COP...Valor.Total),
            qdef=sum(Qtde.Cob.Def),
            ind=sum(Cob.PG...Valor.Corrigido.Total)
            )

chave3<-paste0(
  cop.agr$ANO,
  cop.agr$Tipo.Remessa,
  cop.agr$cd.mun,
  cop.agr$cd.emp)

length(unique(chave3)) #OK
rm(chave3)

base<-full_join(gerais, cop.agr, by = c("ANO","Tipo.Remessa","cd.mun","cd.emp"))
rm(cop.agr)

#juntando base ibge

library(readxl)

ibge<-read_excel("C:/Users/joao.isidio/Desktop/Meso_Micro_Mun.xls")
names(ibge)
names(ibge)[c(1,10,8,6,2)]<-c("COD_CADMU","COD_GEOCODIGO","CODMICRO","CODMESO","COD_UF")
ibge$COD_GEOCODIGO<-as.character(ibge$COD_GEOCODIGO)
ibge$COD_CADMU<-as.character(ibge$COD_CADMU)
ibge<-ibge[,-11]

ibge<-ibge[!duplicated(x = ibge),]

##conversao<-read_excel("//masrv03/DGRR/PAINEL DE CONTROLE - PSR/RECOR-SICOR 08.07.2015/GEOCOD.xlsx")
##names(conversao)

##localizacao<-left_join(ibge,conversao,by="COD_GEOCODIGO")
##localizacao.anti<-anti_join(ibge,conversao,by="COD_GEOCODIGO")
##localizacao.anti
##existem 6 municipios excluidos

localizacao<-ibge
rm(ibge)
#rm(localizacao.anti,ibge,conversao)


names(base)[5]<-"COD_CADMU"
base$COD_CADMU<-as.character(base$COD_CADMU)

base<-inner_join(base, localizacao, by="COD_CADMU")
base.anti<-anti_join(base, localizacao, by="COD_CADMU")
#perdeu-se 39 registros de municipios, mas nenhum tem indenizações. A analise por mesoregião sofrera
#prejuizo
rm(base.anti)

#ciando as novas variveis (ver se tem q fazer no agregado ou no individual)
filtro<-grepl(pattern = "SOJA",x = unique(base$Empreendimento))
sojas<-unique(base$Empreendimento)[filtro]

resul.mun<-
  base %>%
  filter(UF.x == "RS", Empreendimento %in% sojas) %>%
  group_by(COD_GEOCODIGO) %>%
  summarise(SINISTRALIDADE = sum(ind,na.rm = T)/sum(ADICIONAL,na.rm = T), LOSS_COST = sum(ind,na.rm = T)/sum(VALOR.ENQUADR,na.rm = T))

resul.micro<-
  base %>%
  filter(UF.x == "RS", Empreendimento %in% sojas) %>%
  group_by(CODMICRO) %>%
  summarise(SINISTRALIDADE = sum(ind,na.rm = T)/sum(ADICIONAL,na.rm = T), LOSS_COST = sum(ind,na.rm = T)/sum(VALOR.ENQUADR,na.rm = T))

resul.meso<-
  base %>%
  filter(UF.x == "RS", Empreendimento %in% sojas) %>%
  group_by(CODMESO) %>%
  summarise(SINISTRALIDADE = sum(ind,na.rm = T)/sum(ADICIONAL,na.rm = T), LOSS_COST = sum(ind,na.rm = T)/sum(VALOR.ENQUADR,na.rm = T))

resul.uf<-
  base %>%
  filter(UF.x == "RS", Empreendimento %in% sojas) %>%
  group_by(COD_UF) %>%
  summarise(SINISTRALIDADE = sum(ind,na.rm = T)/sum(ADICIONAL,na.rm = T), LOSS_COST = sum(ind,na.rm = T)/sum(VALOR.ENQUADR,na.rm = T))

write.csv2(resul.mun,"soja_rs_mun.csv")
write.csv2(resul.micro,"soja_rs_micro.csv")
write.csv2(resul.meso,"soja_rs_meso.csv")
write.csv2(resul.uf,"soja_rs_uf.csv")
