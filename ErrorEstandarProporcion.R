EEP<-function(x,y){
 #ERROR ESTANDAR DE UNA PROPORCION
 #x=proporcion de sujetos que presentan la caracteristica
 return(sqrt(x*(1-x)/y))
}