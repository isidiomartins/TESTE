foi<-read.csv2("foilista.csv", header=FALSE, stringsAsFactors = FALSE,colClasses = "character")
veio<-read.csv2("veiolista.csv", header=FALSE, stringsAsFactors = FALSE,colClasses = "character")

head(veio)
head(foi)

names(foi)<-"var"
names(veio)<-"var"

outer<-merge(foi,veio,all=T)
natural<-merge(foi,veio,all=F)


library(dplyr)

p1<-anti_join(foi,veio,by="var")
p2<-anti_join(veio,foi,by="var")

p2
