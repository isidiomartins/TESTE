Cadastro<-read.csv2("20160430_Cadastro.csv", sep = "\t", stringsAsFactors = FALSE)

View(Cadastro[Cadastro$NOME=="TOMAS DE SIERVI BARCELLOS",])
rm(Cadastro)
