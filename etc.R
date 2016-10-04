base<-read.csv2("zon_proa.csv")
base<-base[,-1]
View(base)
View(base[base$Geocodigo==4319307,])

library(dplyr)

teste<-
base%>%
  group_by(SafraIni,SafraFin,UF,Cod_Munic,Geocodigo,Nome_Municipio,Cod_Cultura,
           Nome_Cultura,Cod_Solo,Nome_Solo) %>%
  summarise(pontos=min(risco))

View(teste)
View(
teste[teste$Geocodigo==4319307,]
)

teste2<-
teste %>%
  group_by(UF,Nome_Solo,pontos) %>%
  summarise(length(pontos)) %>%
  arrange(Nome_Solo)

write.csv2(teste2,"agregados.csv")
