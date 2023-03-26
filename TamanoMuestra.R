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