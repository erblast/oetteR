


f_get_auc = function(pred, test, plot_rocr = F) {
  
  #untangle test and pred objects
  
  if(is.tibble(pred)){
    pred = pred[[1]]
  }
  
  if(is.tibble(test)){
    test = pred[[1]]
  }
  

  pr = ROCR::prediction( pred, test )
  
  p = ROCR::performance( pr, measure='auc')
  
  if(print == T){
    plot_rocr(p)
  }
  
  return( p@y.values[[1]] )
  
}
