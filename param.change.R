param.change=function(paramlist, new.eta, new.max_depth){
  num=length(paramlist) + 1
  paramlist[[num]]=paramlist[[1]]
  paramlist[[num]]$eta=new.eta
  paramlist[[num]]$max_depth=new.max_depth
  paramlist
}