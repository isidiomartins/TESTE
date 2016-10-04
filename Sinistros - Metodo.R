Deve-se subir ambas as bases, a relativa as linhas que foram encaminhadas as seguradoras, e a relativa as linhas que voltaram das seguradoras. 
Ambas as bases devem ser identicas nas primeiras quatro colunas.

```{r}
foi<-read.csv2(file=choose.files(), stringsAsFactors = FALSE)
veio<-read.csv2(file=choose.files(), stringsAsFactors = FALSE)
```
rm(foi)

C:\Users\joao.isidio\Documents\Envio Seguradoras 11.05.2016
\\masrv03\DGRR\SINISTROS (HISTÓRICO)\2º trabalho - 2016

opcional:
```{r}
veio<-veio[,-5]
foi<-foi[,-1]
```
names(foi)==names(veio)[1:4]

Tanto o outer quanto o natural join devem ter o mesmo conjunto de elemntos.
```{r}
outer<-merge(foi,veio,all=T)
natural<-merge(foi,veio,all=F)
```

Comparação da dimensão.
```{r}
dim(outer)==dim(natural)
```

Comparação das quatro primeiras colunas.
```{r}
identical(foi,veio[,1:4])
```

str(foi)
str(veio)
identical(foi$chave,veio$chave)

olhando coluna a coluna:
```{r}
identical(foi$ANO,veio$ANO) &
identical(foi$CODIGO_SEGURADORA,veio$CODIGO_SEGURADORA) &
identical(foi$NPROP,veio$NPROP) &
identical(foi$NAPOL,veio$NAPOL)
```

O identical aqui seria o suficiente, mas vamos parear linha a linha.
```{r}
foi$chave<-with(foi,paste0(ANO,CODIGO_SEGURADORA,NPROP,NAPOL))
veio$chave<-with(veio,paste0(ANO,CODIGO_SEGURADORA,NPROP,NAPOL))
```

Vejamos se a chave é válida. será se não for possivel reduzir o numero de elemntos.
```{r}
length(unique(foi$chave))==dim(foi)[1]
length(unique(veio$chave))==dim(foi)[1]
length(unique(foi$chave))==length(unique(veio$chave))
```

Ordenemos pela chave:
```{r}
foi<-foi[order(foi$chave),]
veio<-veio[order(veio$chave),]
```

Emparelhadas linha a linha, vejamos a comparação:
```{r}
head(foi$NPROP==veio$NPROP,30)
```

Pela dificuldade de visualizar, contemos os falsos.
```{r}
sum(!foi$ANO==veio$ANO)==0 &
sum(!foi$CODIGO_SEGURADORA==veio$CODIGO_SEGURADORA)==0 &
sum(!foi$NPROP==veio$NPROP)==0 &
sum(!foi$NAPOL==veio$NAPOL)==0
```


names(veio)[4]<-"NAPOL"
veio<-veio[,-7]

View(natural[!foi$NAPOL==veio$NAPOL,])

write.csv2(natural[!foi$NAPOL==veio$NAPOL,],"teste.csv")
write.csv2(veio,"Swiss RE_2.csv")

ALLIANZ:

table(veio$chave)[table(veio$chave)>1]

dim(table(veio$chave))

veio[veio$chave=="20155PSR102035968PSR517720152U010001315",]



A solução é simplesmente colocar as duas colunas da tabela veio na tabela foi.
Entretanto, cabe saber se as linhas são as mesmas.


veio$NPROP<-foi$NPROP
veio$NAPOL<-foi$NAPOL

write.csv2(veio,"Porto.csv")

outer<-merge(foi,veio,all=T)
natural<-merge(foi,veio,all=F)

summary(outer)

foi$chave<-with(foi,paste0(ANO,CODIGO_SEGURADORA,NPROP,NAPOL))
veio$chave<-with(veio,paste0(ANO,CODIGO_SEGURADORA,NPROP,NAPOL))
length(foi$chave)
length(unique(foi$chave))

length(veio$chave)
length(unique(veio$chave))


names(foi)
A allianz tem o numero de linhas distintos!!!

teste<-merge(foi,veio)


[1:31002]

veio[!foi$ANO==veio$ANO[1:31002],]

sum(!foi$ANO==veio$ANO[1:31002])
##################################################################




foi$ANO,veio$ANO



teste<-cbind(
veio$NPROP[!foi$NPROP==veio$NPROP],
foi$NPROP[!foi$NPROP==veio$NPROP]
)

sim, posso colocar nprop no lugar

teste2<-cbind(
  veio$NAPOL[!foi$NAPOL==veio$NAPOL],
  foi$NAPOL[!foi$NAPOL==veio$NAPOL]
)

View(teste2)

View(
cbind(
  nchar(veio$NAPOL[!foi$NAPOL==veio$NAPOL]),
  nchar(foi$NAPOL[!foi$NAPOL==veio$NAPOL])
))

table(nchar(veio$NAPOL[!foi$NAPOL==veio$NAPOL]))
table(nchar(foi$NAPOL[!foi$NAPOL==veio$NAPOL]))


vetor.t<-gsub(pattern = "PSR0",
     replacement = "PSR",
     foi$NAPOL[!foi$NAPOL==veio$NAPOL])
identical(veio$NAPOL[!foi$NAPOL==veio$NAPOL],vetor.t)















dif1<-veio[,3:4][!foi$chave==veio$chave,]
dif2<-foi[,3:4][!foi$chave==veio$chave,]

View(as.data.frame(cbind(dif1,dif2)))



unique(veio$EVENTO)



foi$chave<-with(foi,paste0(ANO,CODIGO_SEGURADORA,NPROP,NAPOL))
veio$chave<-with(veio,paste0(ANO,CODIGO_SEGURADORA,NPROP,NAPOL))



TESTE<-merge(foi,veio, all=FALSE,by = chave)
?merge
