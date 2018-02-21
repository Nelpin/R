require(Rfacebook)
library(RCurl)

token <- "EAACEdEose0cBADkdXrEHaGAvKCWKI8jlAwMJPWuY16Anenjg1hFXS0zADHUGTPe9U4g9OO9vWZAZAypv0eVSypfbsSQVGE1ifpcDPZBdDYVSIPIDuGRoKZAJZBZBVUrBZCW47CG2Ffvmi5uaDcacKUZByYTI4lUM9ZBQZCNyTxPMezOH3taa2hosUbyJZC0DGVYx5IZD"
#Petro
pet<-getUsers('me',token,private_info = T)


grupos<-searchGroup('Ayacucho',token)
gruposId<-grupos$id

pages<-searchPages('Ayacucho',token,n=500)
pagesPeru<-subset(pages,pages$country=='Peru')
pagesId<-pagesPeru$id

dfGrupos<-c(gruposId,pagesId)
dfGrupos<-as.data.frame(dfGrupos)
colnames(dfGrupos)[1] <- "Id"

pagesData<-getPage("gobiernoregionalayacucho", token)

users<-getUsers('170482990399612',token, private_info = F)
posts<-getP

red<-getNetwork(token,format = 'edgelist', verbose = T)


getGrupo<-getGroup('354468688073906',token, n=100)
