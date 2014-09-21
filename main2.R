main2<-function(hiddenvek,seqvek,thrvek,Anzahl){
  data=createData()
  data@mu<-c(75,44,64,70,6,89,38,24,75) #Erwartungswerte
  data@sigma<-c(30,15,40,60,5,40,18,15,30)#Standarabweiung 
  data@probality<-c(7.5,8.32,5.91,0.43,1.36,1.87,6.36,0.6,2.46)#Gewichtung der Einkaeufe
  data<-normProbs(data)
  data@intervall=c(0,200)
  clust=Cluster()
  clust@Profil<-data
  clust=iniCluster(clust,2000)
  
  trainIter=2000
  observations=3
  ret=c()
  for(hiddenstats in hiddenvek){#:10){ 
    model=Model()
    model=iniModel(model,clust,hiddenstats,observations)
    model=trainModel(model,trainIter)
    for(seqsize in seqvek){#1:5 *5){
      for(th in thrvek){  
        ret=c(ret,helpfunc2(model,seqsize,th,Anzahl))
      }
    }
  }
  rete=array(ret,dim = c(1,length(thrvek),length(seqvek),length(hiddenvek)))
  return(rete)
}

helpfunc2<-function(model,seqsize,threshold,Anzahl){
  dekt=Detektion()
  dekt=iniDetektion(dekt,model,seqsize,threshold)
  ret=rep(0,1)
  
  for(s in (3)){
    for(i in 1:Anzahl){
      dekt=addValues(dekt,getValues(model@cluster@Profil,seqsize))
      if(testValue(dekt,model@cluster@center[s]))
        ret[1]=ret[1]+1
    }
  }
  return(ret)
}
