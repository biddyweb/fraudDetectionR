
#Es wichtig die HMM Biblithek zuladen da sie nicht Standart m�ssig geladen wird dies muss vom benutzer angepasst werden
library("HMM", lib.loc="D:/Program Files (x86)/R-3.1.1/library")
#Die folgenden Sourcedateien liegen im selben ordner
source('./createData.R')
source('./Cluster.R')
source('./Modell.R')
source('./Detektion.R')
source('./main.R')


library("plot3D", lib.loc="D:/Program Files (x86)/R-3.1.1/library")
# Die Parameter sind jeweils in Vektoren und haben folgende reihen folge(Anzahl von versteckten Zust�nden,Sequenzl�nge,Schwellwert)
#hinzu kommt noch der optionale Paramter der Testanzahl
erg=main(5:10,1:5 *5,c(0.3,0.5,0.7,0.9),500)
hist3D(z=erg[4,2,,],colvar=NULL,border = "black",phi = 20,theta = -35,ticktype = "detailed")


#Erstellen von Zeitplots
times=TimeFunc()

hist3D(z=times,y = seq(5, 10, length.out = ncol(times)),colvar=NULL,border = "black",phi = 20,theta = -35,ticktype = "detailed",x = seq(5, 25, length.out = nrow(times)),xlab="Sequenzl�nge",ylab="veteckte Zust�nde",zlab="Sekunden pro 1000 Durchl�ufe")
dimnames(times)=list(c(1:5 *5),c(5:10))
barplot(times[3,],xlab = "Anzahl versteckter Zustaende",ylab = "Sekunden pro 1000 Durchl�ufe")
barplot(times[,3],xlab = "Sequenzl�nge",ylab = "Sekunden pro 1000 Durchl�ufe")

