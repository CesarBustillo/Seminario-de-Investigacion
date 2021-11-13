install.packages("lpSolve")
#Libreria necesaria para poder resolver nuestro problema#
library(lpSolve) 

#Matriz de costos para asignar cada repartidor a cada destino#
cost<-matrix(c(66,73,88,76,83,90,48,77,86,75,51,84,50,61,58,
               56,92,96,89,48,43,47,60,46,46,85,64,55,63,44,
               88,42,93,91,80,52,80,79,85,86,99,99,86,40,81), nrow = 3)
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
