#Tratamento da base da Allianz
## Carga e limpeza

Da-se a carga nas tabelas:
```{r}
foi<-read.csv2(file=choose.files(), stringsAsFactors = FALSE)
veio<-read.csv2(file=choose.files(), stringsAsFactors = FALSE)
```

Foi dado um `head(veio)` e constatado que foram trazidas duas colunas avulsas. Retira-se então estas colunas:

```{r}
veio<-veio[,-c(7,8)]
```

Cria-se uma chave para uso futuro:
```{r}
veio$chave<-with(veio,paste0(ANO,CODIGO_SEGURADORA,NPROP,NAPOL))
```

A chave é invalida, pois não é feita de elemtos unicos. O fato é que a empresa desagregou as chaves por eventos (avisos).

```{r}
length(unique(veio$chave))==dim(veio)[1]
```

##Rearrajo: Redução de linhas e frequencias associadas

Estrutura-se uma nova tabela que enxuga os casos em que ha dois eventos iguais para uma mesma chave.

```{r}
library(dplyr)
veio_2<-
veio %>%
  group_by (chave,ANO,CODIGO_SEGURADORA,NPROP,NAPOL,EVENTO) %>%
  summarise (sum(INDENIZAÇÃO,na.rm=T))
```

Cria-se uma matriz de frequencia das chaves para correlacionar com a tabela principal, assim será possivel filtrar aquelas chaves que tem mais de uma aparição.

```{r}
Freq<-table(veio_2$chave)
Corres<-cbind(as.data.frame(names(Freq), stringsAsFactors = FALSE),as.data.frame(as.integer(Freq)))
names(Corres)<-c("chave","Freq")
```

Juntando as tabelas
```{r}
veio_2<-merge(veio_2,Corres)
names(veio_2)[7]<-"INDENIZAÇÃO"
```

Removendo o não mais util:
```{r}
rm(Freq,Corres)
```

##Dividir, Aplicar e Combinar

###Dividir
Separando as tabelas entre a parte que tem chave unica, e a parte que precisa de tratamento:
```{r}
veio_p1<-veio_2[veio_2$Freq==1,]
veio_p2<-veio_2[veio_2$Freq>1,]
```

Tratando a base veio_p2. Verifica-se se há NAs: A resposta é não.
```{r}
sum(is.na(veio_2$INDENIZAÇÃO))
sum(is.na(veio_2$EVENTO))
```

###Aplicar
A função que trará o evento preponderante da respectiva apólice:

```{r}
eve.prep<-function(x,y,z){
  pos<-
    ifelse(
      length(which(x==max(x)))==1,
      which(x==max(x)),
            ifelse('&'(y[1]!="",y[1]!="BASICA"),
                   which(x==max(x))[1],
                        ifelse('&'(y[2]!="",y[2]!="BASICA"),
                        which(x==max(x))[2],which(x==max(x))[3])))
  
  c(z[pos],y[pos])
} 
```

Cria-se uma tabela para acumular os resultados:
```{r}
a<-vector("character",length(unique(veio_p2$chave)))
A<-data.frame(a,a, stringsAsFactors = FALSE)
names(A)<-c("chave","EVENTO_PREPONDERANTE")
rm(a)
```

Realiza-se a analise de cada sub-tabela filtrada por chave:
```{r}
for(i in 1:length(unique(veio_p2$chave))){
  k<-veio_p2[veio_p2$chave==unique(veio_p2$chave)[i],]
  x<-k$INDENIZAÇÃO
  y<-k$EVENTO
  z<-k$chave
  A[i,]<-eve.prep(x,y,z)
}
```

Junta-se o evento preponderante a apólice
```{r}
veio_p2<-merge(veio_p2,A)
```

Agrega-se a indenização pelo valor do evento preponderante.
```{r}
veio_p2_final<-
veio_p2 %>%
  group_by (ANO,CODIGO_SEGURADORA,NPROP,NAPOL,EVENTO_PREPONDERANTE) %>%
  summarise (sum(INDENIZAÇÃO,na.rm=T))
```

Nomeia-se
```{r}
names(veio_p2_final)[6]<-"INDENIZAÇÃO"
```

###Combinar

A tabela p1 precisa estar no mesmo padrão da p2 para q ambas possam juntar-se:
```{r}
veio_p1<-veio_p1[,c(-1,-8)]
names(veio_p1)<-names(veio_p2_final)
```

Juntando
```{r}
veio_final<-rbind(veio_p1,veio_p2_final)
```

Imprimindo
```{r}
write.csv2(veio_final,"Allinz_final.csv")
```
Fim