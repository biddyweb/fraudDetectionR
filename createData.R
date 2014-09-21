createData <- setClass(
  # Set the name for the class
  "createData",
  
  # Define the slots
  slots = c(
    mu = "vector",
    sigma = "vector",
    probality="vector",
    intervall="vector",
    last="numeric",
    repeatf="vector"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    mu = c(500.0),
    sigma = c(50.0),
    probality=c(1),
    intervall=c(0,1000),
    last=0,
    repeatf=rep(1,1)
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
#     if((length(object@mu)!=length(object@sigma)) || (length(object@mu)!=length(object@probality)||(length(object@mu)==0) {
#       return("Error:Unterschiedlich lange Vektoren oder die leere Vektoren")
#     }
#     if(legth(intervall)!= 2)){
#       return("Error:Intervall muss 2 Werte haben")
#     }
#     if(sum!= 1)){#Achtung dies kann gefaehrlich werden bei Bruechen
#       return("Error:Die Summe der Wahrscheinlichkeiten muss 1 ergeben")
#     }
#     return(TRUE)
  }
)



setGeneric(name="IniData",
           def=function(theObject,mu,sigma,probality,repeatf,intervall)
           {
             standardGeneric("IniData")
           }
)


setMethod(f="IniData",
          signature="createData",
          definition=function(theObject,mu,sigma,probality,repeatf,intervall)
          {
            theObject@mu<-mu #Erwartungswerte
            theObject@sigma<-sigma#Standarabweiung 
            theObject@probality<-probality#Gewichtung der Einkaeufe
            theObject<-normProbs(theObject)
            theObject@intervall=intervall
            theObject@repeatf=repeatf
            return(theObject)
          }
          
)






setGeneric(name="normProbs",
           def=function(theObject)
           {
             standardGeneric("normProbs")
           }
)


setMethod(f="normProbs",
          signature="createData",
          definition=function(theObject)
          {
            s=sum(theObject@probality)
            theObject@probality=theObject@probality/s
            return(theObject)
          }
          
)


setGeneric(name="getValues",
           def=function(theObject,anzahl)
           {
             standardGeneric("getValues")
           }
)


setMethod(f="getValues",
          signature="createData",
          definition=function(theObject,anzahl)
          {
            #if(anzahl>=1){
            tmp=c(0,0)
              ret<-c()
              for(i in 1:anzahl){
                tmp=getValue(theObject,tmp[2])
                ret<-c(ret,tmp[1])
              }
              return(ret)
            #}
            #return("muss mindestens ein wert gefragt werden")
          }
)


setGeneric(name="getValue",
           def=function(theObject,last)
           {
             standardGeneric("getValue")
           }
)


setMethod(f="getValue",
          signature="createData",
          definition=function(theObject,last)
          {
            #if(anzahl>=1){
            probas=theObject@probality
            if(last != 0)              
              probas[last]=theObject@probality[last]*theObject@repeatf[last]
            s=sum(probas)
            probas=probas/s
            
            pseudo=runif(1,min=0,max=1)
            i<-1
            while(sum(probas[1:i])<=pseudo){
              i<-i+1
            }
            last=i
            
            ret<-rnorm(1,theObject@mu[i],theObject@sigma[i])
            while(ret>theObject@intervall[2]||theObject@intervall[1]>ret)
              ret<-rnorm(1,theObject@mu[i],theObject@sigma[i])
            return(c(ret,last))
            #}
            #return("muss mindestens ein wert gefragt werden")
          }
)



setGeneric(name="draw",
           def=function(theObject)
           {
             standardGeneric("draw")
           }
)


setMethod(f="draw",
          signature="createData",
          definition=function(theObject)
          {
            #if(anzahl>=1){
            
            Euro<-seq(from=theObject@intervall[1], theObject@intervall[2], by=200/(theObject@intervall[2]-theObject@intervall[1]))
            Prozent<-0
            for(i in 1:length(theObject@mu))
              Prozent<-Prozent+theObject@probality[i]*dnorm(Euro,theObject@mu[i],theObject@sigma[i])
            
            plot(Euro,Prozent, type="l") 
            #}
            #return("muss mindestens ein wert gefragt werden")
          }
)


