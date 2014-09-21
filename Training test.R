timeTraining<-function(hiddenvek){
  observations=3
  ret=c()
  tr=c(1:5)*10
  for(trainIter in tr){
  for(hiddenstats in hiddenvek){
    now=Sys.time()
    model=Model()
    model=iniModel(model,clust,hiddenstats,observations)
    model=trainModel(model,trainIter)
    now2=Sys.time()
    ret=c(ret,as.numeric(now2-now))
  }}
  rete=array(ret,dim = c(length(hiddenvek),length(tr)))
  return(rete)
  
}
