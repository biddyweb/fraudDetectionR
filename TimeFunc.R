timeFunc<-function(){
hiddenvek=5:10
seqvek=1:5 *5
thrvek=0.2
data=createData()
data@mu<-c(75,44,64,70,6,89,38,24,75) #Erwartungswerte
data@sigma<-c(30,15,40,60,5,40,18,15,30)#Standarabweiung 
data@probality<-c(7.5,8.32,5.91,0.43,1.36,1.87,6.36,0.6,2.46)#Gewichtung der Einkaeufe
data<-normProbs(data)
data@intervall=c(0,200)
clust=Cluster()
clust@Profil<-data
clust=iniCluster(clust,2000)

trainIter=200
observations=3
ret=c()
for(hiddenstats in hiddenvek){#:10){ 
  model=Model()
  model=iniModel(model,clust,hiddenstats,observations)
  model=trainModel(model,trainIter)
  for(seqsize in seqvek){#1:5 *5){
    for(th in thrvek){
      dekt=Detektion()
      dekt=iniDetektion(dekt,model,seqsize,th)
      tmp=proc.time()[3]
      for(i in 1:1000){
        value=getValues(model@cluster,1)
        if(testValue(dekt,value)){}         
      }
      ret=c(ret,proc.time()[3]-tmp)
    }
  }
}
ret=array(ret,c(length(seqvek),length(hiddenvek)),c("Sequenzlaenge","Anzahl versteckter Zustaende"))
return(ret)
}