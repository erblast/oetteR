

mod_vis_clust_2_data = function(rea_clust){
  
  reactive({
    rea_clust()$data
  })
}

mod_vis_groups_ui = function(rea_data, selected = NULL){
  
  renderUI({
    
    categoricals = rea_data()$categoricals
    
    if(is.null(selected)) selected = categoricals[1]
    
    inputPanel(
      
      selectInput('group_var_vis_gr'
                  , 'Select Grouping Variable'
                  , choices  = categoricals
                  , selected = selected)
    
    , checkboxInput('group_show_legend'
                    , 'Show Legend'
                    , value = F)  
        
    , sliderInput('group_vis_plot_height_num'
                   , 'Adjust Plot Height Numericals'
                   , min = 100
                   , max = 2000
                   , value = 600)
    
    , sliderInput('group_vis_plot_height_cat'
                   , 'Adjust Plot Height Categoricals'
                   , min = 100
                   , max = 2000
                   , value = 600)
    
    )
    
  })
}

mod_vis_groups_render_numericals = function(input, rea_data, boxcox = F, color_pallette = NULL){
  

  renderPlot(   height = function(){
    
         input$group_vis_plot_height_num
         
    },   expr ={
    
      
    if (boxcox == T){
      numericals = rea_data()$boxcox
      tit = 'Median values of scaled, centered\nand boxcox-transformed numeric variables'
    } else{
      numericals  = rea_data()$numericals
      tit = 'Median values of scaled and centered\nnumeric variables'
    }  
    
    data        = rea_data()$data
    group_var   = input$group_var_vis_gr
    
    facet_formula = paste0('~',group_var) %>%
      as.formula()

    # plot numericals scaled values
    
    d_plot_scale = data %>%
      select( one_of(numericals) ) %>%
      scale(center = T) %>%
      as_tibble() %>%
      bind_cols( data[,group_var]) %>%
      gather(key = 'key', value = 'value', one_of(numericals) )
    
    medians = d_plot_scale %>%
      group_by_('key', group_var) %>%
      summarize_all( median )
    
    
    p =ggplot(medians, aes(x = as.factor(key)
                        , y = value) ) +
      geom_bar( aes( fill = as.factor(key))
                ,stat = 'identity') +
      geom_hline(yintercept = 0
                 ,size = 1) +
      facet_wrap(facet_formula
                 , ncol = 1 ) +
      labs(title = tit)+
      theme(axis.text.x = element_text(angle = 90)
      )
    
    if( input$group_show_legend == F ){
      
      p = p +
        theme( legend.position = 'none')
    }
    
    if( ! is.null(color_pallette) ){
      
      p = p +
        ggplot2::scale_fill_manual( values = color_pallette)
      
    }
    
    name = stringr::str_c( input$sql_or_lib,'_groupvis_numerical')%>%
      stringr::str_c( input$group_var_vis_gr )%>%
      stringr::str_c( 'h',input$group_vis_plot_height_num )
      
      save_plot(plot = p
                , path = input$path
                , name = name
                , yes = input$save
                , excel = input$save_excel
                )
    return(p)
  
  })
    

}

mod_vis_groups_render_categoricals = function(input, rea_data, color_pallette = NULL){
  
  renderPlot(   height = function(){
    
                  input$group_vis_plot_height_cat
                  
                },   expr ={
    
    data          = rea_data()$data
    group_var     = input$group_var_vis_gr
    categoricals  = rea_data()$categoricals %>%
      .[! . == group_var]
    
    facet_formula = paste0('~',group_var) %>%
      as.formula()
    

    d_plot = data %>%
      as_tibble() %>%
      select( one_of( c(categoricals, group_var) ) ) %>%
      gather(key = 'key', value = 'value', one_of(categoricals) )
    
    # to preserve factor order we have to add a sorting column
    
    d_order = data %>%
      as_tibble() %>%
      select( one_of( c(categoricals, group_var) ) )%>%
      mutate_all( as.integer ) %>%
      gather(key = 'key', value = 'order', one_of(categoricals) ) %>%
      mutate( order = stringr::str_c(key, order) )
    
    d_plot = d_plot %>%
      bind_cols( select(d_order, order) )%>%
      arrange( order ) %>%
      # we have to convert value to factor here
      # ggplot will mess up the order if some 
      # facet groups dont have all levels()
      mutate( value = forcats::as_factor(value))
    
    p = ggplot(d_plot ) +
      geom_bar( aes( x = forcats::as_factor(value)
                     , y = ..prop..
                     , fill = key
                     , group = key)
      )+
      facet_wrap(facet_formula
                 ,ncol = 1)+
      labs(title = 'Percentages for each level of each categorical variable',
           fill = 'factors')+
      theme(axis.text.x = element_text(angle = 90)
            )
    
    if(input$group_show_legend == F){
      
      p = p +
        theme( legend.position = 'none')
    }
    
    if( ! is.null(color_pallette) ){
      
      p = p +
        ggplot2::scale_fill_manual( values = color_pallette)
      
    }
    
    name = stringr::str_c( input$sql_or_lib,'_groupvis_categorical')%>%
      stringr::str_c( input$group_var_vis_gr ) %>%
      stringr::str_c( 'h',input$group_vis_plot_height_cat )
      
    save_plot(plot = p
              , path = input$path
              , name = name
              , yes = input$save
              , excel = input$save_excel
              )
    
    return(p)
  
  })
}

mod_vis_groups_render_no_obs = function(input, rea_data, color_pallette = NULL){
  
  
  renderPlot({
    
    group_var     = input$group_var_vis_gr
    data          = rea_data()$data
    
    data[,group_var] = as.factor(data[[group_var]])

    p = ggplot(data, aes_string(x = group_var
                          , fill = group_var)
          ) +
      geom_bar(show.legend = F)
    
    if( ! is.null(color_pallette) ){
      
      p = p +
        ggplot2::scale_fill_manual( values = color_pallette)
      
    }
    
    return(p)
    
  })
}
