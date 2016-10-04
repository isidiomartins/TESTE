Função de autocovariância amostral (FACV) 

FACV<-
  function(x,y){
    v0<-x-mean(x)
    facv<-(v0[1:(length(x)-y)]%*%v0[(1+y):length(x)])/(length(x)-1)
    vetor<-v0[1:(length(x)-y)]*v0[(1+y):length(x)]
    print(facv)
    print(vetor)
  }

y<-4
x<-c(8,14,17,5,11)

for(i in 0:4){
  FACV(x,i)
}

######################################
for(j in seq_along(revistas)){
  for(i in 1:20){
    download.file(enderecos[2,4], destfile=paste("rev_",2,"_pag_",4,".html"))
    goo<-read_html(paste("rev_",2,"_pag_",4,".html"))
    goo %>%
      html_table(fill = TRUE) %>%
      View()
  }}
