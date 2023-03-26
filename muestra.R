M<-function(v,d,p){
  #TAMAÑO DE LA MUESTRA PARA ESTIMAR UNA PROPORCION
  #v=%Za 80% a 99%
  #p=error cometido 0%-9,9% para p=q
  #d=precisión 1-10%
  valor_x<-c(0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99)
  valor_y<-c(1.28,1.65,1.69,1.75,1.81,1.88,1.96,2.054,2.170,2.326,2.576)
  datos<-cbind(valor_x,valor_y)
  v<-v/100
  p<-p/100
  d<-d/100
  za=which(datos==v, arr.ind = TRUE)
  TT<-datos[za,2]
  q<-1-p
  return(TT[1]*TT[1]*p*q/(d*d))
  return(M(v,d,p))
}

Zalfamedios<-function(x){
  return(qnorm((100-x)/200,lower.tail = FALSE))
}
  

Zalfa<-function(x){
  valor_x<-c(0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99)
  valor_y<-c(1.28,1.65,1.69,1.75,1.81,1.88,1.96,2.054,2.170,2.326,2.576) 
  datos<-cbind(valor_x,valor_y)
  za=which(datos==x, arr.ind = TRUE)
  TT<-datos[za,2]
  return(TT[1])
}

MAS<-function(x,y){
  #MUESTREO ALEATORIO SISTEMATICO
  #x=Tamaño de la poblacion
  #y=Tamaño de la muestra
  return(x/y)
}

EEC<-function(x,y){
 #ERROR ESTANDAR DE UNA MEDIA
 #x=Desviacion estandard
 #y=Tamaño de la muestra
 return(x/sqrt(y))
}

EEP<-function(x,y){
 #ERROR ESTANDAR DE UNA PROPORCION
 #x=proporcion de sujetos que presentan la caracteristica
 return(sqrt(x*(1-x)/y))
}

IC<-function(x,y){
 #INTERVALO DE CONFIANZA DE UNA PROPORCION
 #x=prevalencia esperada del parametro a esperar
 #n=tamaño de la muestra
 return(sqrt(x*(1-x)/y))
}

ICM<-function(x,y){
 #INTERVALO DE CONFIANZA DE UNA MEDIA
 #X=desviación estandar
 #n=tamaño de la muestra
  return(x/sqrt(y))
}

Nresumen<-function(x){
 #MUESTRA POR CALCULO RESUMIDO
 #x=diferencia 0..1
 return(round(1/x^2))
}

Nc<-function(x,y){
 #CALCULO  DEL TAMAÑO  DE LA MUESTRA PARA ESTIMAR UNA MEDIA
 #x=tamaño de la muestra
 #y=porcentaje esperado de pérdidas
 return(round(x/(1-y)))
}

CMedias<-function(x,y,z,t){
 #COMPARACION DE MEDIAS
 #x=Zalfa
 #y=Zbeta
 #z=Desviación estandar
 #t=valor mínimo de la diferencia que se desea detectar
 return(round(2*(Zalfa(x)+Zalfa(y))^2*z^2/t^2))
}