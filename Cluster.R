Cluster <- setClass(
  # Set the name for the class
  "Cluster",
  
  # Define the slots
  slots = c(
    Profil = "createData",
    center = "vector",
    group="numeric"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    Profil=createData(),
    center = c(1,2,3),
    group=2
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

setMethod(f="getValue",
          signature="Cluster",
          definition=function(theObject,last)
          {
            #if(center 1 != c(1,2,3)){
              y<-getValue(theObject@Profil,last)

              return(c(clusterValue(theObject,y[1]),y[2]))
          }
)




setMethod(f="getValues",
          signature="Cluster",
          definition=function(theObject,anzahl)
          {
            #if(center 1 != c(1,2,3)){
            i<-1
            last=0
            tmp=getValue(theObject,last)
            res=tmp[1]
            while(i<anzahl){
              i<-i+1
              tmp=getValue(theObject,tmp[2])
              res=c(res,tmp[1])

            }
            return(res)
          }
)




setGeneric(name="iniCluster",
           def=function(theObject,trainCluster)
           {
             standardGeneric("iniCluster")
           }
)



setMethod(f="iniCluster",
          signature="Cluster",
          definition=function(theObject,trainCluster)
          {
            validObject(theObject@Profil)
            pro=theObject@Profil

            t=kmeans(getValues(pro,trainCluster),3,iter.max=60,algorithm="Lloyd")
            n=0
            old=0
            ged=0
            for(i in t$size){
              n<-n+1
              if(i>old){
                old<-i
                ged=n
              }
                
            }

            old=t$centers[ged]
            theObject@center=sort(t$centers)
            n=0
            for(i in theObject@center){
              n<-n+1
              if(i==old){
                theObject@group<-n
              }
            }
            return(theObject)
          }
)


setGeneric(name="clusterValue",
           def=function(theObject,value)
           {
             standardGeneric("clusterValue")
           }
)


setMethod(f="clusterValue",
          signature="Cluster",
          definition=function(theObject,value)
          {
            #if(center 1 != c(1,2,3)){
            y<-value
            
            if( abs(theObject@center[1]-y) <abs(theObject@center[2]-y)) {
              
              if( abs(theObject@center[1]-y) <abs(theObject@center[3]-y)) {
                return(1);
              }
              return(3);
            }
            else{
              if( abs(theObject@center[2]-y) <abs(theObject@center[3]-y)) {
                return(2);
              }
              return(3);
            }
            
            #}
            return("muss mindestens ein wert gefragt werden")
          }
)





