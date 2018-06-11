

require(shiny)
source('f_classification_v03.R')

mod_tree_UI = function(input, status, rea_ana){
  
  renderUI({
    
    validate(
      need(status$group_stat, 'Waiting for Group Stats')
      , need(status$imp, 'Waiting for Predictive Capacity Data')
    )
    
    
    d_clean = rea_ana()
    categoricals = d_clean$categoricals
    numericals   = d_clean$numericals
    
    inputPanel(
      
      selectInput("group_tree"
                  , label    = "select predicted variable"
                  , choices  = c(categoricals, numericals)
                  , selected = input$default_group
                  
      )
      
      , numericInput('min_split'
                     ,label = 'minimum node size to attempt split'
                     , min   = 2
                     , max   = 10000
                     , step  = 1
                     , value = 20
      )
      
      , numericInput('max_depth'
                     , label = 'maximum tree depth'
                     , min   = 2
                     , max   = 30
                     , step  = 1
                     , value = 30
      )
      
      , numericInput('cp'
                     ,label = 'minimum complexity reduction to attempt split'
                     , min   = 0.0001
                     , max   = 0.99
                     , step  = 0.001
                     , value = 0.01
      )
      
      , numericInput('prune'
                     , label = 'Complexitiy(CP) Threshold for Pruning'
                     , min   = 0
                     , max   = 1
                     , step  = 0.1
                     , value = 0
      )
    )
    
  })
  
}

mod_tree_rea = function( input, status, rea_ana ){
  
  reactive({
    
    validate(
      need(status$group_stat, 'Waiting for Group Stats')
      , need(status$imp, 'Waiting for Predictive Capacity Data')
    )
    
    withProgress(message = 'Calculating Decision Tree'
                 ,{
                   
                   d_clean = rea_ana()
                   data = d_clean$data
                   
                   group_var = input$group_tree
                   n_levels  = levels( data[[input$group_tree]]) %>%
                     length()
                   
                   
                   form = stringr::str_c(group_var, '~.') %>%
                     as.formula()
                   
                   m = rpart::rpart(form
                                    , data
                                    , minsplit = input$min_split
                                    , cp       = input$cp
                                    #, maxcompete = 4
                                    #, maxsurrogate = 5
                                    #, usesurrogate = 2
                                    #, xval = 10
                                    #, surrogatestyle = 0
                                    , maxdepth = input$max_depth
                                    , xval     = 10)
                   
                   if( ! input$prune == 0){

                     m = rpart::prune( m, cp = input$prune)
                   }
                   
                   data$pred_class = predict(m, data, type = 'class')
                   data$pred_prob  = predict(m, data, type = 'prob')[,n_levels]

                 })
    
    status$tree = T
    
    return( list(m = m
                 , data = data))
    
  })
}

mod_tree_UI_plot = function(){
  
  inputPanel(
    
    checkboxInput('fallen_leaves'
                  , label = 'fallen_leaves'
                  , value = T)
    
    , numericInput('tweak'
                   ,label = 'increase_text_size'
                   , min   = 0.1
                   , max   = 10
                   , step  = 0.1
                   , value = 1.2
    )
    
    , numericInput('gap'
                   , label = 'gap'
                   , min   = 0
                   , max   = 100
                   , step  = 1
                   , value = 2
    )
    
    , numericInput('space'
                   , label = 'space'
                   , min   = 0
                   , max   = 100
                   , step  = 1
                   , value = 2
    )
    
  )
}

mod_tree_plot = function(input, status, rea_tree){
  
  
  
  tagList(
    
    renderPlot({
      
      validate(
        need(status$group_stat, 'Waiting for Group Stats')
        , need(status$imp, 'Waiting for Predictive Capacity Data')
      )
      
      m = rea_tree()$m
      
      rpart.plot::prp(m
                      , branch.type   = 5
                      , box.palette   ="RdYlGn"
                      , faclen        = 0
                      , extra         = 6
                      , fallen.leaves = input$fallen_leaves
                      , tweak         = input$tweak
                      , gap           = input$gap
                      , space         = input$space
      )
      
      name = stringr::str_c( input$sql_or_lib,'_tree1_', input$group_tree )
      
      save_plot(plot = p
                , path = input$path
                , name = name
                , yes = input$save
                , excel = F
      )
    })
    
    , renderPlot({
      
      validate(
        need(status$group_stat, 'Waiting for Group Stats')
        , need(status$imp, 'Waiting for Predictive Capacity Data')
      )
      
      m = rea_tree()$m
      
      rpart.plot::rpart.plot(m
                             , fallen.leaves = T
                             , tweak         = input$tweak
                             , gap           = input$gap
                             , space         = input$space
      )
      
      name = stringr::str_c( input$sql_or_lib,'_tree2_', input$group_tree )
      
      save_plot(plot = p
                , path = input$path
                , name = name
                , yes = input$save
                , excel = F
      )
    })
    
  )
  
}



mod_tree_prune_plot = function(input, rea_tree, rea_ana){
  
  tagList(
    

    renderPrint( print(rea_tree()$m) )
    
    , DT::renderDataTable({
      
      m_tree = rea_tree()$m
      
      prune_df = tibble( nsplit   = m_tree$cptable[,2]
                         , complexity = m_tree$cptable[,1]
                         , cv_error = m_tree$cptable[,4]
      )
    })
    
    , renderPlot({
      
      m_tree = rea_tree()$m
      
      prune_df = tibble( complexity = m_tree$cptable[,1]
                         , cv_error = m_tree$cptable[,4]
                         , nsplit   = m_tree$cptable[,2]) %>%
        gather( key = 'key', value = 'value', complexity, cv_error)
      
      
      
      ggplot(prune_df, aes(x = nsplit, y = value) ) +
        geom_line()+
        geom_point()+
        facet_wrap(~key, ncol = 1,scales = 'free_y')
      
    })
    
    , renderPrint({
      
      m         = rea_tree()$m
      data      = rea_tree()$data
      group_var = input$group_tree
      
      if( ! is.factor(data[[group_var]]) ) {
        stop('need factor to create prediction table')
      }
      
      predicted = data[['pred_class']]
      control   = data[[group_var]]
              
      print(  table (predicted, control) )
             
      
    })
    
    , renderPrint({
      
      m         = rea_tree()$m
      data      = rea_tree()$data
      group_var = input$group_tree
      
      if( ! is.factor(data[[group_var]]) ) {
        stop('need factor to calculate AUC')
      }
      
      if( length( levels( data[[group_var]] ) ) > 2 ){
        stop('need binary factor to calculate AUC')
      }
      
      predicted = data[['pred_prob']]
      control   = data[[group_var]]
      
      paste('AUC:', f_get_rocr_auc(predicted, control) )
      
    })
    
    , renderPlot({
      
      m         = rea_tree()$m
      data      = rea_tree()$data
      group_var = input$group_tree
      
      if( ! is.factor(data[[group_var]]) ) {
        stop('need factor to calculate AUC')
      }
      
      if( length( levels( data[[group_var]] ) ) > 2 ){
        stop('need binary factor to calculate AUC')
      }
      
      predicted = data[['pred_prob']]
      control   = data[[group_var]]
      
      f_get_rocr_auc(predicted, control, plot_rocr = T) 
    })
  )

  
}



