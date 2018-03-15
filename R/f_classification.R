

#==========================================================================================
# These functions belwo have been written for e1071::tune, which make them a bot outdated,
# nevertheless we might be able to reuse some of the code
#



f_rocr_get_optimal_cut_off_for_spec_tpr = function( rocr_performance_obj, min_tpr = 0.5 ){
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

f_rocr_get_cut_off_for_spec_fpr = function( rocr_performance_obj, max_fpr = 0.5 ){

  find_cut_off = function( cut_off, fpr, tpr, max_fpr ){
    # this function will be used further down in a pmap call
    #

    cut_off = tibble(   cut_off = cut_off
                        , fpr     = fpr
                        , tpr     = tpr) %>%
      filter( fpr <= max_fpr ) %>%
      arrange( desc(fpr) ) %>%
      head(1) %>%
      .$cut_off

  }

  # lists are packed into a dataframe and then pmap is used to apply
  # the worker function, iterating over the tpr, fpr, cut_off values
  cut_off = tibble(   cut_offs   = rocr_performance_obj@alpha.values
                      , fpr     = rocr_performance_obj@x.values
                      , tpr     = rocr_performance_obj@y.values ) %>%
    mutate( cut_off = pmap_dbl( list(cut_offs, fpr, tpr)
                                      , find_cut_off
                                      , max_fpr = max_fpr   )) %>%
    summarise(cut_off = mean(cut_off) ) %>%
    .$cut_off

  return(cut_off)

}

f_get_rocr_auc = function(pred, test, plot_rocr = F) {


  if( ! is.list(pred) ) list(pred)
  if( ! is.list(test) ) list(test)


  pr = ROCR::prediction( pred, test )

  perf_plot =  ROCR::performance( pr, measure='tpr', x.measure = 'fpr' )
  perf_auc  =  ROCR::performance( pr, measure='auc' )

  t = tibble( auc = perf_auc@y.values) %>%
    unnest(auc) %>%
    summarize( mean_auc = mean(auc)
               ,n       = n()
               ,sem_auc = sd(auc) / sqrt( n() )
               ,sd_auc  = sd(auc)
    )

  if(plot_rocr == T) ROCR::plot(perf_plot)


  return( list(   auc                  = t$mean_auc
                , n                    = t$n
                , auc_sem              = t$sem_auc
                , auc_sd               = t$sd_auc
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

  pred = randomForest:::predict.randomForest( m, newdata = data, type = 'prob')[,2]

  #print(head(pred))

  return(pred)

}

f_wr_predict_rpart = function( m, data){

  pred = rpart:::predict.rpart( m, newdata = data, type = 'prob')[,2]

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

f_tune_binary_classification = function( data, formula, .f , .p, cv = 10, min_tpr = 0.5, ranges = NULL){

  pred_var = as.character(formula)[2]

  # make container that provides cost function and stores CV results
  cv_container = f_rocr_cost_make_cv_container()

  # cross validate random forest
  t = e1071::tune( .f
                   , formula
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
  cut_off = f_rocr_get_optimal_cut_off_for_spec_tpr( rocr$rocr_performance_obj, min_tpr = min_tpr)


  print( paste( 'Optimal cut off value:', cut_off ) )


  #contingency table

  tab = f_contingency_table_binary(pred, test, cut_off)

  print(tab)

  m = t$best.model

  m$auc      = rocr$auc
  m$auc_sem  = rocr$auc_sem
  m$cut_off  = cut_off
  m$tab      = tab
  m$rocr     = rocr
  m$pred     = pred
  m$test     = test

  m$prob_case_1_if_pred_0 = tab %>%
    filter( pred == 0) %>%
    group_by(pred, prob_case_1) %>%
    summarise() %>%
    .[['prob_case_1']]

  m$prob_case_1_if_pred_1 = tab %>%
    filter( pred == 1) %>%
    group_by(pred, prob_case_1) %>%
    summarise() %>%
    .[['prob_case_1']]

  return(m)
}

f_contingency_table_binary = function( pred, test, cut_off ){

  if( is.list(pred) ) pred = unlist(pred)
  if( is.list(test) ) test = unlist(test)

  tab = tibble( pred   = pred
                , test = test ) %>%
    mutate( pred   = ifelse( pred > cut_off, 1, 0)
            , test = as.character(test)
            , test = as.integer(test)
    ) %>%
    group_by( pred, test) %>%
    count() %>%
    ungroup() %>%
    mutate( perc = n/sum(n) * 100
            , prob_case_1 = test * n) %>%
    group_by( pred ) %>%
    mutate( prob_case_1 = sum(prob_case_1)/sum(n) )

  return = tab

}

f_random_forest_binary_classification_vis = function(data
                                                     , formula
                                                     , cv      = 10
                                                     , min_tpr = 0.5){

  m = f_tune_binary_classification(data
                                   , formula
                                   , randomForest::randomForest
                                   , f_wr_predict_random_forest
                                   , min_tpr = min_tpr
                                   , cv = cv
                                  )

  #variable importance

  imp = tibble(   mean_decrease_gini = m$importance[,1]
                , variable           = names(m$importance[,1])
                , rank               = min_rank( as.integer( desc(mean_decrease_gini) ) )
                ) %>%
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

  return( list(m = m
               , imp = imp)
          )

}
