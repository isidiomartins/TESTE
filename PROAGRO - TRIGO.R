#Mostra os arquivos no repositório
list.files("//masrv208/Zoneamento Agricola/DADOS BACEN")

#Cria dataframe do PROAGRO-TRIGO
proagro<-read.csv2("//masrv208/Zoneamento Agricola/DADOS BACEN/BCB_DEROP_TRIGO_aa_2004_a_31_03_2016_V00.csv",
                   stringsAsFactors = FALSE)

#Cria visualização
View(head(proagro,200))

#A estrtutura revela que os dados estão quase todos na forma de caracter;
str(proagro) #Os dados deverão ser tradados

#Retorna o tamanho do vetor de elementos unicos de cada variavel
var.elem<-sapply(proagro,function(x){length(unique(x))})
#É vetor
is(var.elem)
#Agora ordenado
var.elem<-var.elem[order(var.elem)]
var.elem

#A função "limpa.num" fará os acertos necessarios nos valores observados como numericos
limpa.num <- function(x) {
  add<- x
  add<-gsub(pattern = "%",replacement = "",x = add)
  add<-gsub(pattern = " ",replacement = "",x = add)
  add<-gsub(pattern = "\\.",replacement = "",x = add)
  add<-gsub(pattern = ",",replacement = ".",x = add)
  add<-as.numeric(add)
  return(add)
}

#A parte da tabela proagro com os números acertados
proagro[, 30:41]<- as.data.frame(lapply(X = proagro[, 30:41],FUN = limpa.num))

#Cria visualização
View(head(proagro,200))

#Agora tratar-se-ão os dados em formato de data
#Eis os campos que tem datas
proagro[,c(4,5,8:13,16,18)]
#Uma visualização desses campos
length(c(4,5,8:13,16,18))
View(head(proagro[,c(4,5,8:13,16,18)],1000))
View(tail(proagro[,c(4,5,8:13,16,18)],1000))

#Procurando o padrão de datas para cada vetor
vetores.data<-sapply(proagro[,c(4,5,8:13,16,18)],unique)
names(vetores.data)

#A formula seguinte traz um resumo de numero de caracteres contido em cada linha do vetor. Grosso modo, os textos de dez caracteres 
#representam as datas que estão no formato correto, o de zero por estar vazio e o de um por ter um valor igual a zero.
#Se forem poucas as situações em que os valores distinguem de 0,1 ou 10 então significa que a fórmula "limpa.data" funcionará bem.

data.formato<-function(x){
  a1<-table(nchar(x))
  a2<-table(x[nchar(x)!=0 & nchar(x)!=1 & nchar(x)!=10])
  return(list(a1,a2))
}

#Eis o teste
lapply(proagro[,c(4,5,8:13,16,18)],data.formato)

#Problemas:
#INI.COLHEITA: 21566 elementos com 6 digitos (de 87276 com 10), alguns com mes e ano (112006), mas a maioria com ano e mes (200610).
#INI.EVENTO: 21564 elementos com 6 digitos (de 87276 com 10), alguns com mes e ano (112006), mas a maioria com ano e mes (200610).

#A solução, parece-me, deva ser adicionar um dia "01" ao fim da string que tenha 6 elementos.
#Fora também reestruturada a string para colocar em formato de data
limpa.data <- function(x) {
  add<-x
  for(i in 1:length(add)){
    if(nchar(add[i])==6){
      add[i]<-paste0(add[i],"01")
      add[i]<-paste0(substring(add[i],first = c(1,5,7) ,last = c(4,6,8))[3],"/",
                     substring(add[i],first = c(1,5,7) ,last = c(4,6,8))[2],"/",
                     substring(add[i],first = c(1,5,7) ,last = c(4,6,8))[1])      
    } else {
      add[i]<-add[i]
    }}
  return(as.Date(add, format="%d/%m/%Y"))
}

#Eis as datas com o dia adicionado e estrutura alterada:
proagro[,c(4,5,8:13,16,18)] <- as.data.frame(lapply(X = proagro[,c(4,5,8:13,16,18)],FUN = limpa.data))

#Eis a viazualização
View(proagro)

names(proagro)

saveRDS(proagro, file = "proagro.rds")
rm(proagro)
proagro<-readRDS("proagro.rds")
