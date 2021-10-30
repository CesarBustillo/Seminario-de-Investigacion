install.packages("lpSolve")
#Libreria necesaria para poder resolver nuestro problema#
library(lpSolve) 

#Matriz de costos para asignar cada repartidor a cada destino#
cost<-matrix(c(63,56,85,43), nrow = 2)
cost

#Funcion que calcula el costo minimo de asignacion#
lp.assign(cost)

#Asignacion de cada repartidor a cada destino#
lp.assign(cost)$solution


?barplot
par(mfrow=c(1,1),bg="white",oma=c(0,3,0,0),mar=c(2,2,2,2))
R1<-cbind(cost)
R1

#Grafico de costos de asignacion#
barplot(R1, beside=T,horiz=T,
        main="Costos de Asignación de repartidor a destino")
