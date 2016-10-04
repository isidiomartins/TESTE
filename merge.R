names(foi)
names(veio)

foi$chave<-with(foi,paste0(ANO,CODIGO_SEGURADORA,NPROP))
veio$chave<-with(veio,paste0(ANO,CODIGO_SEGURADORA,NPROP))

length(unique(foi$chave))


foin<-cbind(foi,rep("T",nrow(foi)))
veion<-cbind(veio,rep("T",nrow(veio)))
names(foin)[6]<-"A"
names(veion)[8]<-"B"


teste<-anti_join(foin, veion, by = NULL)

outer<-merge(foi,veio,all=T,by="chave")

outer<-outer[,c(1:4,6,7,5,8)]

diferencas<-outer[outer$NAPOL.x != outer$NAPOL.y,][,c(5,9)]

write.csv2(diferencas,"dif.csv")


View(outer[!outer$A=="T",])

sum(outer$A=="T",na.rm=T)
sum(outer$B=="T",na.rm=T)

x <- data.frame(k1 = c(NA,NA,3,4,5), k2 = c(1,NA,NA,4,5), data = 1:5)
y <- data.frame(k1 = c(NA,2,NA,4,5), k2 = c(NA,NA,3,4,5), data = 1:5)
merge(x, y, by = c("k1","k2")) # NA's match
merge(x, y, by = "k1") # NA's match, so 6 rows
merge(x, y, by = "k2", incomparables = NA) # 2 rows



novo<-read.csv2("//masrv03/DGRR/SINISTROS (HISTÓRICO)/2º trabalho - 2016/SINISTROS_PSR.csv", stringsAsFactors = FALSE)
velho<-read.csv2("C:/Users/joao.isidio/Desktop/base_qvd.csv", stringsAsFactors = FALSE)
novo<-novo[,-1]

outer<-merge(novo,velho,all=T)
natural<-merge(novo,velho,all=F)

library(dplyr)
K<-names(velho)

choose.files()

p1<-anti_join(novo,velho,by=K)
p2<-anti_join(velho,novo,by=K)
rbind(p1,p2)


unique(proagro$N_PROGRAMA)
saveRDS(calen,file = "calendario.rds")


