IC<-function(x,y){
 #INTERVALO DE CONFIANZA DE UNA PROPORCION
 #x=prevalencia esperada del parametro a esperar
 #n=tamaño de la muestra
 return(sqrt(x*(1-x)/y))
}