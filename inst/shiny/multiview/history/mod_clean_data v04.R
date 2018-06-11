

require(shiny)
require(tidyverse)


mod_clean_ui = function(rea_load){
  
  tagList(
  
    inputPanel(
      
      numericInput('max_no_lvls_fctr'
                   , label = 'Max number of levels for categorical variables'
                   , value = 10
                   , min = 1
                   , step = 1)
      
      , numericInput('min_no_vals_num'
                     , label = 'Minimum number of distinct values for numerical variables'
                     , value = 6
                     , min = 1
                     , step = 1)
      
      , checkboxInput('missing'
                      , label = 'Exclude Missing Values'
                      , value = F)
      
      , checkboxInput('save'
                      , label = 'Save Results'
                      , value = F)
      
    )
    
    , renderUI({
      
      data_load = rea_load()
      
      inputPanel(
        
        sliderInput('sample_size'
                    , label = 'Select Number of Observations'
                    , step  = 1
                    , min   = 0
                    , max   = nrow(data_load)
                    , value = 50000
        )
        
      )
      
    })
    
    , renderUI({
      
      data_load = rea_load()
      
      cols = names(data_load)
      ids  = cols[ endsWith(cols, '_id') |  endsWith(cols, '_ID') ]

      
      splitLayout(
        
        checkboxGroupInput('deselect_cols'
                           , label   = 'Select Variables to exclude'
                           , choices = cols
        )
        
        , checkboxGroupInput('select_ids'
                           , label    = 'Select ID variables'
                           , choices  = names(data_load)
                           , selected = ids
        )
      )
      
      

    })
    
    , inputPanel(
      
      actionButton('but_clean'
                   , label = 'Clean Data')
      
    )
    
  )
  
}

mod_clean_rea = function(input, status, rea_load){ 
  
                 

    eventReactive({input$but_clean
                   }
                  ,{
                    
                    
    withProgress(message = 'Cleaning Data'
                 , value = 0
                 ,{
                   
      data = rea_load() %>%
        sample_n( as.integer(input$sample_size) )
      
      if(! purrr::is_empty(input$deselect_cols) ) {
        data = data %>%
          select( - one_of(input$deselect_cols) )
      }
      
      d_clean = f_clean_data(data
                             ,  as.numeric(input$max_no_lvls_fctr)
                             ,  as.numeric(input$min_no_vals_num)
                             ,  input$missing
                             ,  id_cols = input$select_ids
      )
      

      setProgress(value = 1
                  ,message = 'Done')
      
    })
    
    status$clean = 'Data cleaned.Check Summary and press Analyze button!'

    return(d_clean)
    
    })
                 
}


mod_ana_ui = function(rea_clean, select_grouping_var = F){
  
  tagList(
  
    renderUI(
      
      if(select_grouping_var == T){
        
        inputPanel(
        
        selectInput( 'default_group'
                     , 'Select default grouping variable'
                     , choices = rea_clean()$categoricals
                     )
        
        )
      }
      
    )
    
    , inputPanel(
      
      actionButton('but_ana'
                  , label = 'Analyse Data')
      
    )
    
  )
  
}

mod_ana_rea = function(input, status, rea_clean){ 
  
  eventReactive(input$but_ana
                ,{
                
      withProgress(message = 'Applying BoxCox Transformation'
                   , value = 0
                   ,{  
          
        rea_ana = f_boxcox(rea_clean)
      
      })
      
      status$ana = 'Data prepared for analysis. Scroll Down for results'
          
      return(rea_ana)
                  
  })
}

f_boxcox = function(rea_clean){
  
  require(tidyverse)
  require(stringr)
  require(caret)
  require(forcats)
  
  data          = rea_clean()$data
  numericals    = rea_clean()$numericals
  categoricals  = rea_clean()$categoricals
  all_variables = rea_clean()$all_variables
  
  rea_clean_ob  = rea_clean()

  # Boxcox transform numericals

  if(!is_empty(numericals)){

    data_box_cox = data[, numericals] %>%
      mutate_all( function(x) ifelse(x < 0, 0, x ) ) %>%
      mutate_all( function(x) x + 0.00001 )

    trans = preProcess( as.data.frame(data_box_cox), c('BoxCox'), na.remove = T)

    pred = predict(trans, as.data.frame(data_box_cox) )

    names_transformed = names(pred) %>%
      str_c('_boxcox')

    names(pred) = names_transformed

    data = data %>%
      bind_cols(pred)

    if(!ncol(data[,numericals])
       == ncol(data[,names_transformed])) {
      stop( 'boxcox transformation unsucessfull')
    }

  } else {
    names_transformed = NULL
  }
  
  rea_clean_ob$data   = data
  rea_clean_ob$boxcox = names_transformed
  
  return(rea_clean_ob)
}

f_clean_data = function(data
                        , max_number_of_levels_factors = 10
                        , min_number_of_levels_nums = 6
                        , exclude_missing = F
                        , id_cols = NULL){
  
  
  require(tidyverse)
  require(stringr)
  require(caret)
  require(forcats)
  
  data = data %>%
    as_tibble()
  
  # categorize cols
  
  ids           = id_cols
  
  none_ids      = names(data)[ ! names(data) %in% id_cols ]
  
  numericals    = none_ids[ map_lgl( data[, none_ids], is.numeric)]
  
  categoricals  = none_ids[ ! map_lgl( data[, none_ids], is.numeric)]
  
  all_variables = none_ids
  
  
  
  # drop observations with missing Values
  
  if(exclude_missing == T) {
    
    data = data[stats::complete.cases(data),]
    
    print('missing cases exluded')
  }
  
  # add numericals with less than x distinc values to factors
  
  no_unique_vals = map_dbl(data[, numericals], function(x) length(unique(x)) )
  
  categoricals = c(categoricals, numericals[no_unique_vals <  min_number_of_levels_nums] )
  
  numericals = numericals[ !numericals %in% categoricals ]
  
  
  # convert all non numericals and grouping variables to factors
  
  for (var in categoricals ){
    
    
    data[[var]] = as.factor(data[[var]])
    
  }
  
  # collapse smallest levels into one if factor levels exceed size x
  
  
  no_levels = map_dbl( data[, categoricals], function(x) length( levels(x) ) )  
  
  for (var in names( no_levels[no_levels > max_number_of_levels_factors] )) {
    
    data[[var]] = fct_lump(data[[var]], n = max_number_of_levels_factors)
  }
  
  
  # drop categoricals with only one level
  
  if( !is_empty(categoricals)){
    
    data = data %>%
      mutate_at( vars(one_of(categoricals) ) 
                 , fct_drop )
    
    no_lvl = data[,categoricals] %>%
      summarise_all( function(x) list(levels(x)) ) %>%
      summarise_all( function(x) length(x[[1]]) )
    
    only_one_lvl = categoricals[no_lvl == 1]
    
    if(length(only_one_lvl) > 0 ) {
      data = data %>%
        select( - one_of(only_one_lvl) )
      
      all_variables = all_variables[ !all_variables %in% only_one_lvl ]
      categoricals = categoricals[ no_lvl > 1]
      
    }
  } 
  
  
  # return statement
  
  return( list( data            = data
                , numericals    = numericals
                , categoricals  = categoricals
                , all_variables = all_variables
                , ids           = id_cols
                # , boxcox = names_transformed
  )
  )
}
