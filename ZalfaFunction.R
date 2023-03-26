Zalfa<-function(x){
  valor_x<-c(0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99)
  valor_y<-c(1.28,1.65,1.69,1.75,1.81,1.88,1.96,2.054,2.170,2.326,2.576) 
  datos<-cbind(valor_x,valor_y)
  za=which(datos==x, arr.ind = TRUE)
  TT<-datos[za,2]
  return(TT[1])
}