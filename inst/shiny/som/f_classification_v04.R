
f_rocr_get_optimal_cut_off = function( rocr_performance_obj, min_tpr = 0.5 ){
  # calculates optimal cutoff value by maximizing the ratio between true positives
  # and false positives while preserving a minimum tpr percentage.
  
  f_find_opt_cutoff = function( cut_off, fpr, tpr, min_tpr ){
    # this function will be used further down in a pmap call
    # 
    
    cut_off = tibble(   cut_off = cut_off
                        , fpr     = fpr
                        , tpr     = tpr) %>%
      mutate( ratio = tpr/fpr ) %>%
      filter( tpr >= min_tpr , !is.infinite(ratio) ) %>%
      arrange( desc(ratio) ) %>%
      head(1) %>%
      .$cut_off
    
  }
  
  # lists are packed into a dataframe and then pmap is used to apply
  # the worker function, iterating over the tpr, fpr, cut_off values
  cut_off = tibble(   cut_off   = rocr_performance_obj@alpha.values
                      , fpr     = rocr_performance_obj@x.values
                      , tpr     = rocr_performance_obj@y.values ) %>%
    mutate( optimal_cut_off = pmap_dbl( list(cut_off, fpr, tpr)
                                        , f_find_opt_cutoff
                                        , min_tpr = min_tpr   )) %>%
    summarise(optimal_cut_off = mean(optimal_cut_off) ) %>%
    .$optimal_cut_off
  
  
  
}


f_get_rocr_auc = function(pred, test, plot_rocr = F) {
  
  #convert to vector in case they come in as on-column tibble
  pred = pred %>%
    as.vector() 

  test = test %>%
    as.vector() 
  
  pr = ROCR::prediction( pred, test )
  
  perf_plot =  ROCR::performance( pr, measure='tpr', x.measure = 'fpr')
  
  if(plot_rocr == T){
    
    ROCR::plot(perf_plot)
  }
  
  perf_auc = ROCR::performance( pr, measure='auc')
  
  return( list(                    auc = perf_auc@y.values[[1]]
                , rocr_prediction_obj  = pr 
                , rocr_performance_obj = perf_plot) 
          )
  
}


f_rocr_cost_make_cv_container = function(){
  
  #attributes ----------------------------
  
  cv_pred_results = list()
  cv_test_results = list()
  
  counter         = 1
  
  #methods -------------------------------
  
  store_cv_results = function(pred, test){
    
    col_name_pred = paste0('cv_pred_', counter)
    col_name_test = paste0('cv_test_', counter)
    
    
    cv_pred_results[[col_name_pred]] <<- pred
    cv_test_results[[col_name_test]] <<- test
    
    counter                          <<- counter + 1
    
  }
  
  get_rocr_auc_cost = function( test, pred){
    
    store_cv_results( pred, test)
    
    auc = f_get_rocr_auc( pred, test )$auc
    
    print( paste('ROCR AUC during 1 CV:', auc))
    
    return(  1 - auc )
  }
  
  get_cv_results = function(){
    
    return( list ( pred = cv_pred_results
                   , test = cv_test_results
                   )
    )
    
  }
  
  #returns--------------------------------
  
  return( list( get_rocr_auc_cost = get_rocr_auc_cost
                , get_cv_results  = get_cv_results
                ) 
          )
}

   

f_wr_predict_random_forest = function( m, data){
  
  pred = randomForest:::predict.randomForest( m, data, type = 'prob')[,2]
  
  #print(head(pred))
  
  return(pred)
  
}

f_wr_predict_rpart = function( m, data){
  
  pred = rpart:::predict.rpart( m, data, type = 'prob')[,2]
  
  #print(head(pred))
  
  return(pred)
  
}



f_tree_binary_classification_vis = function(data, formula, prune_cp = NULL, tweak = 2){
  

  m = f_tune_binary_classification( data
                                    , formula
                                    , rpart::rpart
                                    , f_wr_predict_rpart
                                    , cv = 10
                                     )
  
  
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
  

}

f_tune_binary_classification = function( data, formula, .f , .p, cv = 10, ranges = NULL){
  
  pred_var = as.character(formula)[2]
  
  # make container that provides cost function and stores CV results
  cv_container = f_rocr_cost_make_cv_container()
  
  # cross validate random forest
  t = e1071::tune( .f
                   , f
                   , data = data
                   , tunecontrol = e1071::tune.control(sampling = "cross"
                                                       , cross = cv
                                                       , error.fun = cv_container$get_rocr_auc_cost
                   )
                   , predict.func = .p
                   , ranges = ranges
  )
  
  #print auc and sample method of cross validation
  print( t$sampling )
  
  print( paste('ROCR AUC Cross-validated:', 1 - t$best.performance))
  
  
  # get cross validation results
  pred = cv_container$get_cv_results()$pred
  test = cv_container$get_cv_results()$test
  
  # get rocr info and print rocr plot
  rocr = f_get_rocr_auc(pred, test, plot_rocr = T)
  
  # calculate optimal cut off
  cut_off = f_rocr_get_optimal_cut_off( rocr$rocr_performance_obj)
  
  
  print( paste( 'Optimal cut off value:', cut_off ) )
  
  
  #contingency table
  tab = tibble( pred = unlist(pred), test = unlist(test) ) %>%
    mutate( pred = ifelse( pred > cut_off, 1, 0) ) %>%
    group_by( pred, test) %>%
    count() %>%
    ungroup() %>%
    mutate( perc = n/sum(n) * 100 )
  
  print(tab)
  
  m = t$best.model
  
  return(m)
}



f_random_forest_binary_classification_vis = function(data, formula, cv = 10){
  
  m = f_tune_binary_classification(data
                                   , formula
                                   , randomForest::randomForest
                                   , f_wr_predict_random_forest
                                  )
  
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