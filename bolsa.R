ggplot(uva,aes(ANO))+
  geom_bar(aes(weight=INDENIZAÇÃO,fill=EVENTO_PREPONDERANTE))

ggplot(uva,aes(ANO,INDENIZAÇÃO))+
  geom_bar(aes(fill=EVENTO_PREPONDERANTE),stat = "identity")

install.packages("quadprog")
install.packages("tseries")
library(tseries)
library(help="tseries")

get.hist.quote('^BVSP','2001-01-01','2001-12-31') # Indice Bovespa
get.hist.quote('PETR4.SA','2016-06-23','2016-07-20') # Petrobras
get.hist.quote('QCOM','2016-06-23','2016-07-20') # Petrobras
get.hist.quote(instrument = "^gspc", start = "1998-01-01", quote = "Close")

?get.hist.quote

sony<-get.hist.quote(instrument = 'SON1.SG',start = '2016-01-01')
micro<-get.hist.quote(instrument = 'MSFT.SW',start = '2016-01-01', quote = "Close")
nintendo<-get.hist.quote(instrument = 'NTDOY',start = '2016-01-01')

games<-cbind(sony,micro,nintendo)


View(games)

library(ggplot2)

Close.sony,Close.micro,Close.nintendo

ggplot(games)+
  geom_line(aes(index(games),Close.sony),color="blue",size=2)+
  geom_line(aes(index(games),Close.micro),color="green",size=2)+
  geom_line(aes(index(games),Close.nintendo),color="red",size=2)+
  ylab("Preço de Fechamento")+
  xlab("Tempo")+
  ggtitle(label = "Microsoft, Sony e Nintendo - 2016")

petr<-diff(petro4$Close)

ggplot(petr, aes(index(petr),petr))+
  geom_line()+
  geom_boxplot()+
  geom_smooth(method="lm")+
  ylab("PETR4.SA - Preço de Fechamento")+
  xlab("Tempo")+
  ggtitle("PETR4.SA")

ggplot(games)+
  geom_line(aes(index(games),Close.sony),color="blue",size=2)