library("HMM", lib.loc="D:/Program Files (x86)/R-3.1.1/library")
#Die folgenden Sourcedateien liegen im selben ordner
source('C:/Users/User/Desktop/Bachlorarbeit/R-program/createData.R')
source('C:/Users/User/Desktop/Bachlorarbeit/R-program/Cluster.R')
source('C:/Users/User/Desktop/Bachlorarbeit/R-program/Modell.R')
source('C:/Users/User/Desktop/Bachlorarbeit/R-program/Detektion.R')
source('C:/Users/User/Desktop/Bachlorarbeit/R-program/main.R')


erg=main(5:10,1:5 *5,c(0.3,0.4,0.5),500)
save(erg, file = "C:/Users/User/Desktop/Bachlorarbeit/grosseDatei.RData")