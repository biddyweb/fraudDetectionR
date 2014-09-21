returnData <- setClass(
  # Set the name for the class
  "Cluster",
  
  # Define the slots
  slots = c(
    ergebnis = "array",
    time="numeric"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    ergebnis=array(0,dim = c(4,1,1,1)),
    time=0
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