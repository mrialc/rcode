
#Función para determinar la mediana de una v.a. discreta
mediana <-function(x){
suma<-0
#vector<-c(2,4,5,8,1,9,21,17,19,14,13)
for (i in 1:length(x)) {
    suma<-suma+x[i]
  }
  m=suma/2
#  print("Mediana=")
#  print(suma/2)

suma<-0
i<-1
  while (suma<m)
  {
    suma<-suma+x[i]
    i<-i+1
  }
print("Mediana -> ")
print(m)
print("La posición del vector en el que se encuentra la Mediana es:")
print(i-1)
print("que se corresponde con el valor:")
print(x[i-1])
}