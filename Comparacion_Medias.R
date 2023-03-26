CMedias<-function(x,y,z,t){
 #COMPARACION DE MEDIAS
 #x=Zalfa
 #y=Zbeta
 #z=Desviación estandar
 #t=valor mínimo de la diferencia que se desea detectar
 return(round(2*(Zalfa(x)+Zalfa(y))^2*z^2/t^2))
}