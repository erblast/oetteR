



f_pca = function( data_ls
                  , use_boxcox_tansformed_vars = T
                  , center = T
                  , scale = T
                  ){
  

   data          = data_ls$data
   boxcox        = data_ls$boxcox
   categoricals  = data_ls$categoricals
   all_variables = data_ls$all_variables
   numericals    = data_ls$numericals
   
   if(is_empty(numericals) | is_empty(boxcox)) return()
   
   if(use_boxcox_tansformed_vars) {
     
     data = boxcox 
     
   } else{
     
     data = data %>%
       select( one_of(numericals) )
   }
   
   pca = prcomp(x        = data 
                , scale. = scale
                , center = center)
   
   pca$cos2 = pca$rotation^2
   
   pca$contrib = lmap( as_tibble(pca$cos2 ), function(y) y/ sum(y) *100 )
   row.names(pca$contrib) = row.names(pca$cos2)
   
   pca$vae = as_tibble (t( pca$sdev / sum(pca$sdev) *100 ) )
   colnames(pca$vae) = colnames(pca$contrib)
   
   pca$contrib_abs_perc = t( t( apply( pca$contrib/100, 1, function(x,y) x*pca$vae ) ) )
   pca$contrib_abs_perc = unnest( as.data.frame( pca$contrib_abs_perc) )
   row.names(pca$contrib_abs_perc) = row.names(pca$contrib)
   
   pca$contrib_abs_perc = as.data.frame( t(pca$contrib_abs_perc) )
   pca$contrib_abs_perc$var = row.names(pca$contrib_abs_perc)
   
   pca$contrib_abs_perc = pca$contrib_abs_perc %>%
     gather(key = 'key', value = 'value', everything(), -var) 
   
   #group variables with less than 2.5% contribution
   pca$contrib_abs_perc_reduced = pca$contrib_abs_perc  %>%
     mutate(key = ifelse(value < 2.5
                         , ' sum contrib < 2.5%'
                         , key)) %>%
     group_by( var, key) %>%
     summarise( value = sum(value) )
   
   
   # filter principle components that explain less than
   # 2.5% of the variance
   
   pca$x                        = pca$x[, pca$vae > 2.5]
   
   pca$contrib_abs_perc         = pca$contrib_abs_perc %>%
     filter(var %in% colnames(pca$x))
   
   pca$contrib_abs_perc_reduced = pca$contrib_abs_perc_reduced %>%
     filter(var %in% colnames(pca$x))
   
   data = data %>%
     cbind(pca$x)
                 


  return(list(data = data, pca = pca) )
  
}


f_pca_get_plot_components = function(pca_ls
                                 , x_axis = 'PC1'
                                 , y_axis = 'PC2'
                                 , group = NULL){
  
  data = pca_ls$data
  pca  = pca_ls$pca
  

  p = ggplot(data) +
    geom_point( aes_string(x = x_axis, y = y_axis, color = group)
                ,alpha=0.4 ) +
    labs(title = 'Principle Components')
  
  #convert rotation matrix to tibble and join contribution in percent
  tib = pca$rotation %>%
    as_tibble() %>%
    mutate( var = row.names( pca$rotation ) ) %>%
    gather( key = 'key', value = 'rotation',  - var) %>%
    left_join( pca$contrib_abs_perc, by = c( 'key' = 'var', 'var' = 'key' )) %>%
    rename( abs_contrib_perc = value )
    
    
  
  #print contrib x_axis
  tib %>%
    filter( key == x_axis) %>%
    arrange( desc(abs_contrib_perc) ) %>%
    select(var, abs_contrib_perc, rotation) %>%
    head(20) %>%
    print()
  

  #print contrib y_axis
  tib %>%
    filter( key == y_axis) %>%
    arrange( desc(abs_contrib_perc) ) %>%
    select(var, abs_contrib_perc, rotation) %>%
    head(20) %>%
    print()
  
  
  return(p)
  
}

f_pca_get_plot_variance_explained = function(pca_ls){
  
  pca = pca_ls$pca
  
  vae = tibble( value = pca$sdev
                , pca_n = str_c('pca', 1:length(pca$sdev))
  ) 
  
  p =   ggplot(pca$contrib_abs_perc_reduced) +
    geom_bar(aes(x = fct_reorder(var, value, sum, .desc = T )
                 , y = value, fill = key)
             , stat = 'identity'
             , position='stack') +
    scale_fill_brewer(palette = 'Paired') +
    labs( title = 'Variance Explained')

  return(p)
  
}
