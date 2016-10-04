to<-read.csv2("Soja - TO - Embrapa - solo arenoso.csv", header = TRUE, stringsAsFactors = FALSE)
View(to)
names(to)

library(dplyr)

unique(to$Cod_Ciclo)

TO1<-to %>% 
  filter(Cod_Ciclo == 20) %>%
  mutate(r = paste(formatC(decendio_inicial, width=2, flag="0"),"a",formatC(decendio_final, width=2, flag="0"))) %>%
  select(Nome_Municipio,r) %>%
  group_by(Nome_Municipio) %>%
  summarise("SOLOS TIPO 1" = ifelse(length(r)==1, r[1],
                                    paste(r[1],"+",r[2])))
write.csv2(TO1,"TO1.csv")

TO2<-to %>% 
  filter(Cod_Ciclo == 21) %>%
  mutate(r = paste(formatC(decendio_inicial, width=2, flag="0"),"a",formatC(decendio_final, width=2, flag="0"))) %>%
  select(Nome_Municipio,r) %>%
  group_by(Nome_Municipio) %>%
  summarise("SOLOS TIPO 1" = ifelse(length(r)==1, r[1],
                                    paste(r[1],"+",r[2])))
write.csv2(TO2,"TO2.csv")

TO3<-to %>% 
  filter(Cod_Ciclo == 22) %>%
  mutate(r = paste(formatC(decendio_inicial, width=2, flag="0"),"a",formatC(decendio_final, width=2, flag="0"))) %>%
  select(Nome_Municipio,r) %>%
  group_by(Nome_Municipio) %>%
  summarise("SOLOS TIPO 1" = ifelse(length(r)==1, r[1],
                                    paste(r[1],"+",r[2])))
write.csv2(TO3,"TO3.csv")