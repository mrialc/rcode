Nc<-function(x,y){
 #CALCULO  DEL TAMAÑO  DE LA MUESTRA PARA ESTIMAR UNA MEDIA
 #x=tamaño de la muestra
 #y=porcentaje esperado de pérdidas
 return(round(x/(1-y)))
}