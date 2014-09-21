library("plot3D", lib.loc="D:/Program Files (x86)/R-3.1.1/library")
#bennen der dimensionen
dimnames(erg)=list(c('org',1,2,3),c(0.3,0.5,0.7,0.9),c(1:5 *5),c(5:10))
erg=erg/500





#erstellen von Prozentualler Werte verteilung
library("HMM", lib.loc="D:/Program Files (x86)/R-3.1.1/library")
source('C:/Users/User/Desktop/Bachlorarbeit/R-program/createData.R')
source('C:/Users/User/Desktop/Bachlorarbeit/R-program/Cluster.R')
source('C:/Users/User/Desktop/Bachlorarbeit/R-program/Modell.R')
source('C:/Users/User/Desktop/Bachlorarbeit/R-program/Detektion.R')
source('C:/Users/User/Desktop/Bachlorarbeit/R-program/main.R')
data=createData()
data@mu<-c(75,44,64,70,6,89,38,24,75) #Erwartungswerte
data@sigma<-c(30,15,40,60,5,40,18,15,30)#Standarabweiung 
data@probality<-c(7.5,8.32,5.91,0.43,1.36,1.87,6.36,0.6,2.46)#Gewichtung der Einkaeufe
data<-normProbs(data)
data@intervall=c(0,200)
clust=Cluster()
clust@Profil<-data
clust=iniCluster(clust,2000)

ret=rep(0,3)
for(i in 1:10000){
  value=getValues(clust,1)
  ret[value]=ret[value]+1
}
ret=ret/10000
ret=array(ret,dim = c(1,3))
dimnames(ret)=list(c('Prozent'),c(1,2,3))
barplot(ret,xlab = "Gruppe",ylab = "Prozentanteil der Gruppen",colvar=NULL,border = "black")









#plot gleichbleibedews verhalten bei verschiedenen Sequenzlänge
barplot(erg[4,1,,2],xlab = "Sequenzlänge",ylab = "Prozentanteil für Warnungen")



  




hist3D(z=times,y = seq(5, 10, length.out = ncol(times)),colvar=NULL,border = "black",phi = 20,theta = -35,ticktype = "detailed",x = seq(5, 25, length.out = nrow(times)),xlab="Sequenzlänge",ylab="veteckte Zustände",zlab="Sekunden pro 1000 Durchläufe")
dimnames(times)=list(c(1:5 *5),c(5:10))
barplot(times[3,],xlab = "Anzahl versteckter Zustaende",ylab = "Sekunden pro 1000 Durchläufe")