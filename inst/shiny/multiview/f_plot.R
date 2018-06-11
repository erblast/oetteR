

f_get_col_vector60 = function(){
  
  library(RColorBrewer)
  n <- 60
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  
}


f_plot_hist = function(variable
                       , data_ls
                       , group = NULL
                       , graph_type = 'violin'
                       , y_axis = 'count'
                       , auto_range = T
                       , n_breaks = 30
                       , x_min = 0
                       , x_max = 100 ){
  
  data          = data_ls$data
  categoricals  = data_ls$categoricals
  all_variables = data_ls$all_variables
  numericals    = data_ls$numericals
  
  x_min = min
  x_max = max

  
  if(variable %in% numericals){
    print('variable in numericals')
  }

  if(variable %in% categoricals){
    print('variable in categoricals')
  }

  #y-axis
  if(y_axis == 'density') {
    y_axis = '..density..'
  }  else{
    y_axis = '..count..'
  }
  
  #group
  
  if(group == 'None') group = NULL 
  

  #geom_freqpoly 
  if(variable %in% numericals & graph_type == 'line'){
    
    p = data %>%
      ggplot() +
      geom_freqpoly( aes_string(x = variable, y = y_axis, color = group)
                     , bins =  as.numeric(n_breaks))
  }
  
  
  #geom_histo 
  if(variable %in% numericals & graph_type == 'bar' ){
    
    
    p = data %>%
      ggplot() +
      geom_histogram( aes_string(x = variable, y = y_axis, fill = group)
                      , bins =  as.numeric(n_breaks), alpha = 0.6
                      , position = 'identity')
  }
  
  #geom_violin 
  if(variable %in% numericals & graph_type == 'violin'){
    
    medians = data %>%
      group_by_(as.symbol(group)) %>%
      select( one_of(numericals ) ) %>%
      summarise_all(  median )
    
    
    p = data %>%
      ggplot() +
      geom_violin( aes_string(x = group
                              , y = variable
                              , fill = group)
      ) +
      geom_crossbar( data = medians,
                     mapping = aes_string(x = group
                                          , y = variable
                                          , ymin = variable
                                          , ymax = variable) )
  }
  
  
  # add x range
  
  if(variable %in% numericals & auto_range == F & !graph_type == 'violin'){
    
    p = p +
      xlim( c( as.numeric(x_min), as.numeric(x_max)) )
  }
  
  # add y range
  
  if(variable %in% numericals & auto_range == F & graph_type == 'violin'){
    
    p = p +
      ylim( c( as.numeric(x_min), as.numeric(x_max)) )
  }
  
  
  # categoricals ----------------------------------------------------------------------------
  
  #geom_bar
  if(variable %in% categoricals ){
    
    if(y_axis == '..density..')   y_axis = '..prop..'
    
    p = data %>%
      ggplot() +
      geom_bar( aes_string(x = variable, y = y_axis, fill = group, group = group)
                , position = 'dodge') +
      theme( axis.text.x = element_text( angle = 90 ) )
    
    
  }
  
  y_axis_str = stringr::str_extract_all(y_axis
                                        , '[A-Za-z]') %>%
    unlist()%>%
    stringr::str_c(collapse = '')
  


  print(p)
  
}


f_plot_time = function(variable
                       , time_variable
                       , data_ls
                       , .f = mean
                       , time_variable_as_factor = F
                       , group = NULL
                       , normalize = F
                       , time_unit = 'day'){
  
  data       = data_ls$data
  numericals = data_ls$numericals
  

  time_variable_sym  <- as.name(time_variable)
  variable_sym       <- as.name(variable)
  group_sym          <- as.name(group)
  
  #make sure variables are in correct format

  if( time_variable_as_factor == T){

    data = data %>%
      mutate( !!time_variable_sym := as.factor(!!time_variable_sym) )

  } else {

    data = data %>%
      mutate( !!time_variable_sym    := as.character(!!time_variable_sym)
               , !!time_variable_sym := as.numeric(!!time_variable_sym) )

  }

  if(!variable %in% numericals){
    stop('variable not in numericals')
  }

  # normalize data if appliquable
  
  if(normalize == T){
    data = data %>%
      filter( (!!time_variable_sym) != 0) %>%
      mutate( !!variable_sym := (!!variable_sym) / (!!time_variable_sym) )

    y_title = paste('Median of', variable, 'per', time_unit)

  }else{
    y_title = paste('Median of', variable)
  }
  
  boxplot_sum = data %>%
    select( !!time_variable_sym, !!variable_sym, !!group_sym ) %>%
    group_by( !!time_variable_sym, !!group_sym ) %>%
    nest() %>%
    arrange( !!time_variable_sym, !!group_sym)%>%
    mutate( data            = map( data, variable )
            , boxplot       = map( data, boxplot.stats) 
            , boxplot_stats = map( boxplot, 'stats') 
            , box_min       = map_dbl( boxplot_stats, function(x) x[1] )
            , box_min_box   = map_dbl( boxplot_stats, function(x) x[2] )
            , box_median    = map_dbl( boxplot_stats, function(x) x[3] )
            , box_max_box   = map_dbl( boxplot_stats, function(x) x[4] )
            , box_max       = map_dbl( boxplot_stats, function(x) x[5] )
    ) %>%
    select( - boxplot_stats, - boxplot )
  
  p = ggplot(boxplot_sum, aes_string( x      = time_variable
                                     , y     = 'box_median'
                                     , color = group )
  )+
    geom_line( size = 1 )+
    geom_crossbar( aes_string( ymin  = 'box_min_box'
                              , ymax = 'box_max_box'
                              , fill = group
                )
                , stat     = 'identity'
                , alpha    = 0.1
                , position = 'identity') +
    geom_errorbar( aes_string( ymin  = 'box_median'
                              , ymax = 'box_median'
                            ) 
                , size = 1) +
    geom_point( size = 2 ) +
    labs( y = y_title
          , x = time_unit
          , subtitle = 'Boxes denote upper and lower 25% percentile') 
  
  print(p)
  
}






