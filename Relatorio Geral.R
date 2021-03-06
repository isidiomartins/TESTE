---
title: "Relat�rio Geral - Proagro (Trigo)"
author: "Jo�o Isidio Freitas Martins"
date: "Segunda, 6 de Junho de 2016"
output: html_document
---

Como rapidamente pode-se perceber, por meio do comando `View(proagro)`, a base fornecida � cheia de valores NA (n�o avaliaveis) para a maioria 
dos vetores (variaveis). Isso implica, algumas vezes, em trabalhar com subconjuntos dos dados analisados.

O primeiro desafio em quest�o � dispor da frequ�ncia de avisos por m�s e ano, tendo a variavel INI.EVENTO (data do �nicio do evento) como
refer�ncia. que fique claro que n�o se estar� apresentando aqui a totalidade dos contratos, nem mesmo sequer a totalidade dos que tiveram avisos.

Deve-se ent�o carregar a base tratada do proagro:

```{r}
proagro<-readRDS("proagro.rds")
```

A f�mula abaixo cria um novo dataframe especifico para a an�lise. A estrutura � flexivel na medida que permite que no futuro possam ser 
adicionadas outras variveis da base proagro.

```{r}
sub1<-data.frame(ind=proagro$R..SINISTRO,
                mes=format(proagro$INI.EVENTO, "%m"),
                ano=format(proagro$INI.EVENTO, "%Y"),
                evento=proagro$N_EVENTO,
                area=proagro$AREA_ha)
sub1<-sub1[!is.na(sub1$ano),]
sub1<-sub1[sub1$ano!=1020,]
```

Obs: � interesante que as variav�is "proagro$R..SINISTRO" e "proagro$AREA_ha" n�o tenham valores NA. A variavel de �rea sequer tem valor 
zero (o que, diga-se de passagem, � muito razoavel). O que justifica a n�o exclus�o de mais linhas.

```{r}
table(proagro$AREA_ha==NA)
table(proagro$AREA_ha==0)
table(proagro$R..SINISTRO==NA)
```

Iniciemos os gr�ficos:

```{r}
library(ggplot2)
```

Com ciclos de plantio e de eventos climaticos bem definidos, era de se esperar um comportamento padronizado nos avisos. Os eventos se concentram
no m�s de setembro:

```{r}
ggplot(sub1, aes(x=mes,fill=ano)) +
  geom_bar() +
  ggtitle("N�mero de Contratos com Aviso\npor M�s e Ano")+
  xlab("M�s do In�cio do Evento")+
  ylab("N�mero de Contratos")
```

Apresenta-se aqui os graf�cos anuais em separado:

```{r}
ggplot(sub1, aes(x=mes,fill=ano)) +
  geom_bar() +
  facet_wrap(~ano) +
  ggtitle("N�mero de Contratos com Aviso\npor M�s e Ano")+
  xlab("M�s do In�cio do Evento")+
  ylab("N�mero de Contratos")
```

Afim de simplificar a analise, nos concentremos nos �ltimos 5 anos.

```{r}
sub2<-sub1[sub1$ano==2011:2016,]
```

Pode-se repetir os graf�cos anteriores para esse novo conjunto de anos:

```{r}
ggplot(sub2, aes(x=mes,fill=ano)) +
  geom_bar() +
  ggtitle("N�mero de Contratos com Aviso\npor M�s e Ano\n2011-2015")+
  xlab("M�s do In�cio do Evento")+
  ylab("N�mero de Contratos") 
```

```{r}
ggplot(sub2, aes(x=mes,fill=ano)) +
  geom_bar() +
  facet_wrap(~ano) +
  ggtitle("N�mero de Contratos com Aviso\npor M�s e Ano\n2011-2015")+
  xlab("M�s do In�cio do Evento")+
  ylab("N�mero de Contratos") 
```

Agora os mesmos graficos com a indeniza��o nas escalas:

```{r}
ggplot(sub2, aes(x=mes,y=ind, fill=ano)) +
  geom_bar(stat = "identity") +
  ggtitle("Valor Agregado da Indeniza��o\npor M�s e Ano\n2011-2015")+
  xlab("M�s do In�cio do Evento")+
  ylab("Indeniza��o - R$") 
```

```{r}
ggplot(sub2, aes(x=mes,y=ind, fill=ano)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ano) +
  ggtitle("Valor Agregado da Indeniza��o\npor M�s e Ano\n2011-2015")+
  xlab("M�s do In�cio do Evento")+
  ylab("Indeniza��o - R$") 
```

Que tal desagregar a informa��o por evento. Mais uma vez, para toda s�rie hist�rica.

```{r}
ggplot(sub1, aes(x=mes,fill=evento)) +
  geom_bar() +
  ggtitle("N�mero de Contratos com Aviso\npor M�s e Ano")+
  xlab("M�s do In�cio do Evento")+
  ylab("N�mero de Contratos")
```

Agora somente para os �ltimos 6 anos:
  
```{r}
ggplot(sub2, aes(x=mes,fill=evento)) +
  geom_bar() +
  ggtitle("N�mero de Contratos com Aviso\npor M�s e Ano")+
  xlab("M�s do In�cio do Evento")+
  ylab("N�mero de Contratos")+
  theme(legend.position="top")
``` 





```{r}
ggplot(sub1, aes(x=mes,fill=evento)) +
  geom_bar() +
  facet_wrap(~ano)+
  ggtitle("N�mero de Contratos com Aviso\npor M�s e Ano")+
  xlab("M�s do In�cio do Evento")+
  ylab("N�mero de Contratos")
```


ggplot(sub1, aes(x=mes, y=ind)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = ano))+
  facet_wrap(~ano)+
  ggtitle("Indeniza��o de contratos com aviso\ndistribuidos por m�s e ano")+
  xlab("M�s do In�cio do Evento")+
  ylab("Indeniza��o")


ggplot(sub1, aes(x=mes,y=ind)) +
  geom_boxplot()
  geom_bar() +
  geom_line() +
  ggtitle("N�mero de contratos com aviso\ndistribuidos por m�s e ano") +
  xlab("M�s do In�cio do Evento") +
  ylab("N�mero de Contratos")

names(proagro)

  geom_bar(stat = "identity",aes(fill = ano.ini.event), position = "dodge") +
  facet_wrap(~ano.ini.event)

ggplot(sub1, aes(x=ind,colour=ano)) +
geom_histogram()

View(proagro[proagro$R..SINISTRO>0,])
proagro[55399,41]<-


Nota: Apresentar por decendio
length(proagro)

tapply(sub$indeniza�ao,c(sub$ano.ini.event,sub$mes.ini.event),sum)

summary(sub)



rm(t)
t<-proagro$INI.EVENTO==1020-07-01
t[t==T]
proagro[proagro$INI.EVENTO==1020-07-01,]
1020-07-01" "1020-07-01"
proagro$INI.EVENTO[order(format(proagro$INI.EVENTO, "%Y"))][1:100]
sum(proagro$INI.EVENTO==as.Date("1020-07-01"))
