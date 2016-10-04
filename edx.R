A<-read.csv(
  paste0(
    getwd(),
    "/Module 2/module2_fiji_workfile_eviews.csv"))
names(A)
dim(A)

#equation one
eq1<-lm(log(A$rgdp_fiji) ~ log(A$rgdp_aus))
plot(
summary(eq1)$residuals
)

library(ggplot2)

ggplot(A,aes(y=rgdp_fiji,x=rgdp_aus))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

e<-as.data.frame(summary(eq1)$residuals)
names(e)<-"erro"

ggplot(e, aes(x=1:44,y=erro))+
  geom_point()+
  geom_line()

#equation two
eq2<-lm(log(A$rgdp_fiji) ~ log(A$rgdp_aus)+log(A$sugaroutput_fiji))
e<-as.data.frame(summary(eq2)$residuals)
names(e)<-"erro"

ggplot(e, aes(x=1:24,y=erro))+
  geom_point()+
  geom_line()

#equation tree
dummy<-rep(0,51)
dummy[38:41]<-rep(1,4)
A$dummy<-dummy

eq3<-lm(log(A$rgdp_fiji) ~ log(A$rgdp_aus)+
                           log(A$sugaroutput_fiji)+
                           A$dummy)
summary(eq3)

e<-as.data.frame(summary(eq3)$residuals)
names(e)<-"erro"

ggplot(e, aes(x=1:24,y=erro))+
  geom_point()+
  geom_line()

#equation 4
eq4<-lm(log(A$rgdp_fiji) ~ log(c(NA,A$rgdp_fiji[1:50]))+
                           log(A$rgdp_aus)+
                           log(c(NA,A$rgdp_aus[1:50]))+
                           log(A$sugaroutput_fiji)+
                           A$dummy)
summary(eq4)

e<-as.data.frame(summary(eq4)$residuals)
names(e)<-"erro"

ggplot(e, aes(x=1:24,y=erro))+
  geom_point()+
  geom_line()

#test 2.5
Dominican<-read.csv(
  paste0(getwd(),
    "/Module 2/Caribbean Islands Data/Module2_Caribbean_Assessment_File_Dominican.csv"),
  skip = 2,stringsAsFactors=FALSE,header=FALSE)
names(Dominican)<-read.csv(
  paste0(getwd(),
         "/Module 2/Caribbean Islands Data/Module2_Caribbean_Assessment_File_Dominican.csv"),
  skip = 0,stringsAsFactors=FALSE,header=FALSE)[1,]

Jamaica<-read.csv(
  paste0(getwd(),
         "/Module 2/Caribbean Islands Data/Module2_Caribbean_Assessment_File_Jamaica.csv"),
  skip = 2,stringsAsFactors=FALSE,header=FALSE)
names(Jamaica)<-read.csv(
  paste0(getwd(),
         "/Module 2/Caribbean Islands Data/Module2_Caribbean_Assessment_File_Jamaica.csv"),
  skip = 0,stringsAsFactors=FALSE,header=FALSE)[1,]
names(Jamaica)[1]<-"ANO"

Bahamas<-read.csv(
  paste0(getwd(),
         "/Module 2/Caribbean Islands Data/Module2_Caribbean_Assessment_File_Bahamas.csv"),
  skip = 2,stringsAsFactors=FALSE,header=FALSE)
names(Bahamas)<-read.csv(
  paste0(getwd(),
         "/Module 2/Caribbean Islands Data/Module2_Caribbean_Assessment_File_Bahamas.csv"),
  skip = 0,stringsAsFactors=FALSE,header=FALSE)[1,]

Trinidad<-read.csv(
  paste0(getwd(),
         "/Module 2/Caribbean Islands Data/Module2_Caribbean_Assessment_File _Trinidad.csv"),
  skip = 2,stringsAsFactors=FALSE,header=FALSE)
names(Trinidad)<-read.csv(
  paste0(getwd(),
         "/Module 2/Caribbean Islands Data/Module2_Caribbean_Assessment_File _Trinidad.csv"),
  skip = 0,stringsAsFactors=FALSE,header=FALSE)[1,]

Atlantis<-read.csv(
  paste0(getwd(),
         "/Module 2/Caribbean Islands Data/Module2_Caribbean_Assessment_File_Atlantis.csv"),
  skip = 2,stringsAsFactors=FALSE,header=FALSE)
names(Atlantis)<-read.csv(
  paste0(getwd(),
         "/Module 2/Caribbean Islands Data/Module2_Caribbean_Assessment_File_Atlantis.csv"),
  skip = 0,stringsAsFactors=FALSE,header=FALSE)[1,]

reg<-lm(
diff(log(Bahamas$bah_cpi_eop)) ~
diff(log(Jamaica$jam_cpi_eop))
)

summary(reg)$residuals


#test 2.6

s<-Jamaica$jam_cpi_eop
length(s)
length(diff(s))
S<-diff(s)/s[1:34]

S<-ts(data = S,start = 1981,end = 2014)
plot.ts(S)
max(S)

#test 2.7
philips<-lm(
Atlantis$atl_infl ~
Atlantis$atl_unemp_rate)

summary(philips)
