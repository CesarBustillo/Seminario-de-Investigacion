install.packages("TransP")
library(TransP)

costo_matrix=data.frame(Destino1=c(66,73,88,40),Destino2=c(76,83,90,25),Destino3=c(48,77,86,36),
                        Destino4=c(75,51,84,25),Destino5=c(50,61,58,15),Destino6=c(56,92,96,23),
                        Destino7=c(89,48,43,60),Destino8=c(47,60,46,17),Destino9=c(46,85,64,58),
                        Destino10=c(55,63,44,33),Destino11=c(88,42,93,76),Destino12=c(91,80,52,44),
                        Destino13=c(80,79,85,65),Destino14=c(86,99,99,12),Destino15=c(86,40,81,48), Oferta=c(147,228,202,577)
                        ,row.names=c("Kennedy ","Cascadas","Miraflores","Demanda"))


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

ex_matrix=data.frame(Destino1=c(66,73,88,40),Destino2=c(76,83,90,25),Destino3=c(48,77,86,36),
                     Destino4=c(75,51,84,25),Destino5=c(50,61,58,15),Destino6=c(56,92,96,23),
                     Destino7=c(89,48,43,60),Destino8=c(47,60,46,17),Destino9=c(46,85,64,58),
                     Destino10=c(55,63,44,33),Destino11=c(88,42,93,76),Destino12=c(91,80,52,44),
                     Destino13=c(80,79,85,65),Destino14=c(86,99,99,12),Destino15=c(86,40,81,48), Oferta=c(147,228,202,577)
                     ,row.names=c("Kennedy ","Cascadas","Miraflores","Demanda"))

ex_matrix 
nwc(ex_matrix)

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