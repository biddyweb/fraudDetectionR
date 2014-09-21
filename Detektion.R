Detektion <- setClass(
  # Set the name for the class
  "Detektion",
  
  # Define the slots
  slots = c(
    Profil = "Model",
    sequenz = "vector",
    threshold="numeric",
    seqsize = "numeric",
    Plausibility= "numeric"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    Profil=Model(),
    seqsize =5,
    threshold=0.3
    
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    #if((object@n < 1) || (object@ncp < 0)) {
    # return("Illigale nummer")
    #}
    return(TRUE)
  }
)


setGeneric(name="iniDetektion",
           def=function(theObject,model,seqsize,threshold)
           {
             standardGeneric("iniDetektion")
           }
)



setMethod(f="iniDetektion",
          signature="Detektion",
          definition=function(theObject,model,seqsize,threshold)
          {
            theObject@seqsize=seqsize
            theObject@threshold=threshold
            theObject@Profil=model
            theObject@sequenz=getValues(model@cluster,seqsize)
            theObject@Plausibility=sum(exp(forward(theObject@Profil@mod$hmm,theObject@sequenz)[,seqsize]))
            return(theObject)
          }
)

setGeneric(name="testValue",
           def=function(theObject,value)
           {
             standardGeneric("testValue")
           }
)


#gibt TRUE zurück wenn es ein gefälschterwert ist
setMethod(f="testValue",
          signature="Detektion",
          definition=function(theObject,value)
          {
            value=clusterValue(theObject@Profil@cluster,value)
            seq=theObject@sequenz
            size=theObject@seqsize
            seq[1:size-1]=seq[2:size]
            seq[size]=value

            value=sum(exp(forward(theObject@Profil@mod$hmm,seq)[,size]))

            value=(theObject@Plausibility-value)/theObject@Plausibility

            if(value>theObject@threshold)              
              return(TRUE)
            else
              return(FALSE)
          }
)

size=10
seq=getValues(clust,(size+1000))
testv11=c()
testvo1=c()
for(i in (size+1):(size+1000)){
  testv11=c(testv1,sum(exp(forward(model@mod$hmm,seq[(i-size):i])[,size])))
  testvo1=c(testvo,sum(exp(forward(modelorg,seq[(i-size):i])[,size])))
}



setGeneric(name="addValue",
           def=function(theObject,value)
           {
             standardGeneric("addValue")
           }
)




setMethod(f="addValue",
          signature="Detektion",
          definition=function(theObject,value)
          {
            value=clusterValue(theObject@Profil@cluster,value)
            seq=theObject@sequenz
            size=theObject@seqsize
            seq[1:size-1]=seq[2:size]
            seq[size]=value
            theObject@sequenz=seq
            theObject@Plausibility=sum(exp(forward(theObject@Profil@mod$hmm,seq)[,size]))
            
            return(theObject)

          }
)

setGeneric(name="addValues",
           def=function(theObject,values)
           {
             standardGeneric("addValues")
           }
)




setMethod(f="addValues",
          signature="Detektion",
          definition=function(theObject,values)
          {
            for(i in values)
              theObject=addValue(theObject,i)
            return(theObject)
            
          }
)





