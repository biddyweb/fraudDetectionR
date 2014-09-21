main<-function(hiddenvek,seqvek,thrvek,Anzahl){
  data=createData()
  mu<-c(75,44,64,70,6,89,38,24,75) #Erwartungswerte
  sigma<-c(30,15,40,60,5,40,18,15,30)#Standarabweiung 
  probality<-c(7.5,8.32,5.91,0.43,1.36,1.87,6.36,0.6,2.46)#Gewichtung der Einkaeufe
  intervall=c(0,200)
  repeatf=c(2,0.4,1.5,1.2,0.2,5,3,1.4,3)
  data=IniData(data,mu,sigma,probality,repeatf,intervall)
  clust=Cluster()
  clust@Profil<-data
  clust=iniCluster(clust,2000)

  trainIter=2000
  observations=3
  ret=c()
  
  for(hiddenstats in hiddenvek){#:10){ 
    print("trainingsPhase")
    model=Model()
    model=iniModel(model,clust,hiddenstats,observations)
    model=trainModel(model,trainIter)
    
    for(seqsize in seqvek){#1:5 *5){
      print("sequenz")
      for(th in thrvek){  
        ret=c(ret,helpfunc(model,seqsize,th,Anzahl))
      }
    }
  }
  rete=array(ret,dim = c(4,length(thrvek),length(seqvek),length(hiddenvek)))
  return(rete)
}

helpfunc<-function(model,seqsize,threshold,Anzahl){
  dekt=Detektion()
  dekt=iniDetektion(dekt,model,seqsize,threshold)
  ret=rep(0,4)
  
  for(i in 1:Anzahl){
    dekt=addValues(dekt,getValues(model@cluster@Profil,seqsize))
    value=getValues(model@cluster,1)
    if(testValue(dekt,value))
      ret[1]=ret[1]+1
  
       for(s in (1:3)){
      if(testValue(dekt,model@cluster@center[s]))
          ret[1+s]=ret[1+s]+1
  }
  }
  return(ret)
}
