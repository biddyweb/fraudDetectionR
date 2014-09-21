Model <- setClass(
  # Set the name for the class
  "Model",
  
  # Define the slots
  slots = c(
    cluster = "Cluster",
    mod = "list"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    
    mod=initHMM(c("1.st","2.st","3.st"),c("l","m","h"))
    
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

setGeneric(name="iniModel",
           def=function(theObject,cluster,hidden,emission)
           {
             standardGeneric("iniModel")
           }
)



setMethod(f="iniModel",
          signature="Model",
          definition=function(theObject,cluster,hidden,emission)
          {
            theObject@cluster=cluster
            x<-seq(1,emission,1)
            d=c(dnorm(x,1+(2*(1-1)/(emission-1)),0.75))
            fx<-d/sum(d)
            sum=0
            for (i in 2:hidden){
              d=c(dnorm(x,1+(2*(i-1)/(hidden-1)),0.75))
              fx<-c(fx,d/sum(d))
            }
            theObject@mod=initHMM(1:hidden, 1:emission,rep(1/hidden,hidden),# 
                                  #(matrix(rep(1/(hidden*2),hidden*hidden),hidden)+1/2*diag(hidden)),
                                  emissionProbs=t(matrix(fx,nrow=emission)))
#             theObject@mod=initHMM(1:hidden, 1:emission,rep(1/hidden,hidden), 
#                                   matrix(rep(1/(hidden),hidden*hidden),hidden),
#                                   matrix(rep(1/(emission),emission*hidden),emission))  
            return(theObject)
          }
)


setGeneric(name="trainModel",
           def=function(theObject,anzahl)
           {
             standardGeneric("trainModel")
           }
)



setMethod(f="trainModel",
          signature="Model",
          definition=function(theObject,anzahl)
          {
            theObject@mod = baumWelch(theObject@mod,getValues(theObject@cluster,anzahl),15)
            return(theObject)
          }
)

