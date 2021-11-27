library(TransP)

costo_matrix=data.frame(Destino1=c(38,50,50,50),Destino2=c(38,38,38,35),Destino3=c(38,38,38,46),
                        Destino4=c(38,50,50,35),Destino5=c(38,50,50,25),Destino6=c(38,50,50,33),
                        Destino7=c(38,50,50,70),Destino8=c(38,50,50,27),Destino9=c(38,50,50,68),
                        Destino10=c(38,50,50,43),Destino11=c(38,50,50,86),Destino12=c(38,50,50,54),
                        Destino13=c(38,50,50,75),Destino14=c(38,50,50,22),Destino15=c(38,50,50,58), Oferta=c(157,238,212,587)
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
