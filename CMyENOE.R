install.packages("TransP")
library(TransP)

costo_matrix=data.frame(CANADA=c(63,85,35),ALEMAN=c(56,43,30), OFERTA=c(20,15,35)
                        ,row.names=c("KENNEDY","CASCADAS","DEMANDA"))

costo_matrix 
mincost(costo_matrix)

mincost=function(ex_matrix){
  
  if(sum(is.na(ex_matrix))>0)
    stop("Your matrix has NA values")
  
  Demand=as.vector(ex_matrix[nrow(ex_matrix),-ncol(ex_matrix)])
  Supply=as.vector(ex_matrix[-nrow(ex_matrix),ncol(ex_matrix)])
  High_Values=max(ex_matrix) + 999999999
  Alloc_Matrix=ex_matrix[-nrow(ex_matrix),-ncol(ex_matrix)]
  ex_matrix=Alloc_Matrix
  Alloc_Matrix[,]=0
  Total_Cost=0
  Total_alloc=0
  
  while(sum(Supply) != 0 & sum(Demand) != 0)
  {
    tc=which.min(apply(ex_matrix,MARGIN=2,min))  
    tr=which.min(apply(ex_matrix,MARGIN=1,min))  
    
    min_curr=min(Demand[tc],Supply[tr])
    
    Demand[tc]=Demand[tc] - min_curr
    Supply[tr]=Supply[tr] - min_curr
    Alloc_Matrix[tr,tc]=min_curr
    Total_Cost=Total_Cost+(min_curr*ex_matrix[tr,tc])
    
    if(Demand[tc]==0)
    {
      ex_matrix[,tc]=rep(High_Values,nrow(ex_matrix))
    }else if(Demand[tc]==Supply[tr])
    {
      ex_matrix[tr,]=rep(High_Values,ncol(ex_matrix))
      ex_matrix[,tc]=rep(High_Values,nrow(ex_matrix))
    }else{
      ex_matrix[tr,]=rep(High_Values,ncol(ex_matrix))
    }
    Total_alloc=Total_alloc+1
  }
  
  output=list()
  output$Alloc_Matrix=Alloc_Matrix
  output$Total_Cost=Total_Cost
  
  #Si la oferta y demanda no son iguales
  if(sum(Demand) != 0)
    output$Dummy_demand=sum(Demand)
  else if(sum(Supply) != 0)
    output$Dummy_supply=sum(Supply)
  
  if(Total_alloc < (nrow(Alloc_Matrix) + ncol(Alloc_Matrix)-1))
    warning("Degenracy in Transporation Problem Occurred")
  return(output)
}

ex_matrix=data.frame(CANADA=c(63,85,35),ALEMAN=c(56,43,30), OFERTA=c(20,15,35)
                     ,row.names=c("KENNEDY","CASCADAS","DEMANDA"))

ex_matrix #Una matriz de costos donde la última columna debe ser la oferta y la última fila debe ser la demanda. La matriz de entrada no debe tener valores perdidos (NA), de lo contrario, la función arrojará un error#
nwc(ex_matrix)#Esta función implementa el algoritmo North-West Corner para resolver el problema de transporte mediante una matriz de costos optimizada y un costo total optimizado#

nwc=function(ex_matrix){
  
  if(sum(is.na(ex_matrix))>0)
    stop("Your matrix has NA values")
  
  Alloc_Matrix=ex_matrix[-nrow(ex_matrix),-ncol(ex_matrix)]
  Alloc_Matrix[,]=0
  tr=1
  tc=1
  Total_Cost=0
  Total_alloc=0
  colnames(ex_matrix)[ncol(ex_matrix)]="Supply"
  while(sum(ex_matrix[nrow(ex_matrix),]) != 0 & sum(ex_matrix[,ncol(ex_matrix)]) != 0)
  {
    min_curr=min(ex_matrix[tr,ncol(ex_matrix)],ex_matrix[nrow(ex_matrix),tc])
    ex_matrix[tr,ncol(ex_matrix)]=ex_matrix[tr,ncol(ex_matrix)] - min_curr
    ex_matrix[nrow(ex_matrix),tc]=ex_matrix[nrow(ex_matrix),tc] - min_curr
    Alloc_Matrix[tr,tc]=min_curr
    Total_Cost=Total_Cost+(min_curr*ex_matrix[tr,tc])
    if(ex_matrix[nrow(ex_matrix),tc]==0)
    {
      tc=tc+1
    }else if(ex_matrix[tr,ncol(ex_matrix)]==ex_matrix[nrow(ex_matrix),tc])
    {
      tr=tr+1
      tc=tc+1
    }else{
      tr=tr+1
    }
    ex_matrix[nrow(ex_matrix),ncol(ex_matrix)]=sum(ex_matrix$Supply[-nrow(ex_matrix)])
    Total_alloc=Total_alloc+1
  }
  
  output=list()
  output$Alloc_Matrix=Alloc_Matrix
  output$Total_Cost=Total_Cost
  
  #Si la oferta y demanda no son iguales
  if(sum(ex_matrix[nrow(ex_matrix),]) != 0)
    output$Dummy_demand=sum(ex_matrix[nrow(ex_matrix),])
  else if(sum(ex_matrix[,ncol(ex_matrix)]) != 0)
    output$Dummy_supply=sum(ex_matrix[,ncol(ex_matrix)])
  
  if(Total_alloc < (nrow(Alloc_Matrix) + ncol(Alloc_Matrix)-1))
    warning("Degenracy in Transporation Problem Occurred")
  
  return(output)
}