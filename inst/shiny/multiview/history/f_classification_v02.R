


f_get_rocr_auc = function(pred, test, plot_rocr = F) {
  

  pred = pred %>%
    as.vector() %>%
    as.numeric()

  test = test %>%
    as.vector() %>%
    as.numeric()
  
  pr = ROCR::prediction( test, pred )
  
  
  if(plot_rocr == T){
    
    p =  ROCR::performance( pr, measure='tpr', x.measure = 'fpr')
    ROCR::plot(p)
  }
  
  p = ROCR::performance( pr, measure='auc')
  
  return( p@y.values[[1]] )
  
}

f_rocr_cost = function(test, pred){
  # a wrapper for f_get_rocr_auc to use as a cost function for 
  # e1071::tune().
  
  # the arguments come in in reverse
  auc = f_get_rocr_auc( pred, test )
  
  print( paste('ROCR AUC during 1 CV:', auc))
  
  return(  1 - auc )
  
}

f_wr_predict_random_forest = function( m, data){
  
  pred = randomForest:::predict.randomForest( m, data, type = 'prob')[,2]
  
  #print(head(pred))
  
  return(pred)
  
}

f_tree_binary_classification_vis = function(data, formula, prune_cp = NULL, tweak = 2){
  
  pred_var = as.character(formula)[2]
  
  m = rpart::rpart(formula = formula, data = data)
  
  
  prune_df = tibble( complexity = m$cptable[,1]
                     , cv_error = m$cptable[,4]
                     , nsplit   = m$cptable[,2])
  
  print(prune_df)
  
  p = prune_df %>%
    gather( key = 'key', value = 'value', complexity, cv_error) %>%
    ggplot( aes(x = nsplit, y = value) ) +
    geom_line()+
    geom_point()+
    facet_wrap(~key, ncol = 1,scales = 'free_y')
  
  print(p)
  
  
  if(!is.null(prune_cp)){
  
    m = rpart::prune( m, cp = prune_cp)
    
  }
  
  print(m)
  
  rpart.plot::prp(m
                  #, branch.type   = 5
                  , box.palette   ="RdYlGn"
                  , faclen        = 0
                  , extra         = 106
                  , fallen.leaves = F
                  , under         = T
                  , tweak = tweak
  )
  
  #  type = 'prob' returns a dataframe with one column for each level
  #  we have to index to get columns for prediction = 1
  predicted_prob  = predict(m, data, type = 'prob')[,2]
  predicted_class = predict(m, data, type = 'class')
  
  test      = data[[pred_var]]
  
  table( predicted_class, test ) %>%
    print()
  
  auc = f_get_rocr_auc( predicted_prob, test, plot_rocr = T)
  
  print( paste('ROCR AUC:', auc))
  
}


f_random_forest_vis = function(data, formula, cv = 10){
  
  pred_var = as.character(formula)[2]
  
  # cross validate random forest
  t = e1071::tune( randomForest::randomForest
                   , f
                   , data = data
                   , tunecontrol = e1071::tune.control(sampling = "cross"
                                                       , cross = cv
                                                       , error.fun = f_rocr_cost
                                                        )
                   , predict.func = f_wr_predict_random_forest
                   
                  )
  
  #print auc and sample method of cross validation
  print( t$sampling )
  
  print( paste('ROCR AUC Cross-validated:', 1 - t$best.performance))
  
  
  # get best model
  m = t$best.model
  
  
  # test on whole dataset
  predicted_prob  = predict(m, data, type = 'prob')[,2]
  predicted_class = predict(m, data, type = 'class')
  
  test      = data[[pred_var]]
  
  #contingency table
  table( predicted_class, test ) %>%
    print()
  
  #auc
  auc = f_get_rocr_auc( predicted_prob, test, plot_rocr = T)
  
  print( paste('ROCR AUC not validated:', auc))
  
  
  #variable importance
  imp = tibble( mean_decrease_gini = m$importance[,1]
                ,variable = names(m$importance[,1]) ) %>%
    arrange(desc(mean_decrease_gini))
  
  p = ggplot(imp, aes(  x    = fct_reorder(   variable
                                            , mean_decrease_gini) 
                      , y    = mean_decrease_gini
                      ) 
             )+
    geom_col( aes(fill = fct_reorder(  variable
                                       , mean_decrease_gini
                                       , .desc = T))
              , show.legend = F)+
    coord_flip()
  
  print(p)
  
  print(imp)
  
}