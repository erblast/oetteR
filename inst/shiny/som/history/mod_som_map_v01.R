
require(shiny)


mod_trans_som_rea = function(rea_ana){
  
  reactive({
    
    withProgress(message = 'Preparing Data for SOM Map'
                 ,{
    
      data          = rea_ana()$data
      numericals    = rea_ana()$numericals
      boxcox        = rea_ana()$boxcox
      categoricals  = rea_ana()$categoricals
    
      
      data_list = list()
      distances = vector()
      
      for (fac in categoricals){
        
        data_list[[fac]] = kohonen::classvec2classmat( data[[fac]] )
        
        distances = c(distances, 'tanimoto')
        
        incProgress( 1/length(categoricals) )
        
      }
      
      data_list[['numericals']] = scale(data[,boxcox])
      distances = c( distances, 'euclidean')
    
    })
    
    return( list(data_list      = data_list
                 ,distances     = distances
                 , numericals   = numericals
                 , boxcox       = boxcox
                 , categoricals = categoricals))
      
  })
  
}

mod_train_map_som_rea = function(rea_trans_som
                                 , rea_clean
                                 , input
                                 , status){
  
  eventReactive({input$but_train_map
                 input$checkbox_use_uploaded_map
                 }
               ,{ 
    
    if(input$checkbox_use_uploaded_map == T) {
      
      status$map_trained = 'Using Loaded Map'
        
      return(NULL)   
    }         
                 
    start_time = lubridate::now()
    
    withProgress( message = paste('map training started at', start_time)
                  ,{
     
    
      data_list     = rea_trans_som()$data_list
      numericals    = rea_trans_som()$numericals
      categoricals  = rea_trans_som()$categoricals
      distances     = rea_trans_som()$distances
      
      # setting user weights leads to asynchoneous training of the map
      # see documentation 
      # weights = c( length(numericals), rep( 1, length(categoricals) ) )
      
      som_grid = kohonen::somgrid(xdim   = input$map_dim_a
                               , ydim = input$map_dim_b
                               , topo ="hexagonal")
      
      
      m = kohonen::supersom( data_list
                          , grid=som_grid
                          , rlen= input$n_iter
                          , alpha = 0.05
                          , whatmap = c(categoricals
                                        , 'numericals')
                          , dist.fcts = distances
                          #, user.weights = weights
                          #, maxNA.fraction = .5
                        )
    
    })
    
    end_time = lubridate::now()
    
    out_str = paste('Map training took'
                    , difftime(end_time, start_time, units = 'mins') %>%
                      round(1)
                    ,'min')
    
    status$map_trained = out_str
    
    
    trained_som = list(data   = rea_clean()
                       , map  = m
                       , grid = som_grid)
    
    return( trained_som )
    
  })

}



mod_som_map_ui = function(){
  
  inputPanel(
    
    numericInput('map_dim_a'
                    ,label = 'Map Dimension a'
                    , min = 5
                    , max = 1000
                    , value = 20
                    , step = 1)
    
    ,numericInput('map_dim_b'
                   ,label = 'Map Dimension b'
                   , min = 5
                   , max = 1000
                   , value = 20
                   , step = 1)
    
    ,numericInput('n_iter'
                   ,label = 'No of training iterations'
                   , min = 5
                   , max = 100000
                   , value = 500
                   , step = 1)
    
    ,fileInput('upload_map'
               , label = 'Upload Map')
    
    
    ,checkboxInput('checkbox_use_uploaded_map'
                   ,label = 'Use uploaded map'
                   #,value = F
                  )

    ,actionButton('but_train_map'
                 ,label = 'Train map'
    )
  )
  
}


mod_som_map_exec_time_out = function(rea_trans_som, input, status){
  
  # we are using two different reactive functions
  # not to trigger the recalculation if only no of
  #training iterations are changed
  
  rea_time = reactive({
    
    withProgress(message = 'Estimating training time'
                 ,{
      
      n_test_iterations = 5
      
      data_list     = rea_trans_som()$data_list
      numericals    = rea_trans_som()$numericals
      categoricals  = rea_trans_som()$categoricals
      distances     = rea_trans_som()$distances
      
      som_grid = kohonen::somgrid(xdim   = input$map_dim_a
                                  , ydim = input$map_dim_b
                                  , topo ="hexagonal")
      
      t_before = lubridate::now()
      
      m = kohonen::supersom( data_list
                             , grid=som_grid
                             , rlen= n_test_iterations
                             , alpha = 0.05
                             , whatmap = c(categoricals
                                           , 'numerics')
                             , dist.fcts = distances
                             #, maxNA.fraction = .5
      )
      
      t_after = lubridate::now()
      
      t_diff = difftime(t_after,t_before, units = 'mins') %>%
        as.numeric()
      
      return(t_diff)
    })
    
  })

  reactive({
    
    t_diff = rea_time()
    
    out_str = paste('Map will take approximately'
                    , round(t_diff * input$n_iter/5,1)
                    , 'min to train')
    
    status$est_exec_time = out_str
    
  })
    
  
  
  
}

mod_save_map_som_rea = function(rea_trained_som
                                , input
                                , status
                                ){
  dat = lubridate::now() %>%
    lubridate::date()%>%
    as.character()%>%
    stringr::str_replace_all('-','')
  
  f_save = function(file){
    
    withProgress( message = 'saving map'
                 ,{
    
      save_list = rea_trained_som()
      save( save_list, file = file)
    
    })
    
  }
  
  file_name = stringr::str_c('som_map_'
                             ,dat,
                             '.Rdata')
  
  #in a markdown document no downloadButton needs to be
  #created. outputArgs takes kwargs for the layout 
  #of the button, however the outputID is aumatically
  #assigned in this case dont pass it in outputArgs.
  
  
  inputPanel(
  
    downloadHandler(filename  =  file_name
                    , content = f_save
                    , outputArgs = list(label = 'Save Map'))
  
  )
  
}


mod_load_map_rea = function(rea_som_trained
                            , input
                            , status){
  
  reactive({
    
     som = rea_som_trained()
    
    if( input$checkbox_use_uploaded_map == T 
        & !is.null(input$upload_map) ) {
      
      print('loading_map')
      
      load(input$upload_map$datapath)
      
      if(!'save_list' %in% ls()
         & ! 'map' %in% names(save_list)
         & ! 'data' %in% names(save_list)
         & ! 'grid' %in% names(save_list)){
        
        stop('corrupted map uploaded')
      }
      
      som = save_list
      
      status$map_loaded = 'Map Loaded'
    }
  
    return(som)  
    
  })
  

}

mod_som_mao_plot = function(input, rea_som){
  
  tagList(
    
    renderPlot({
      
      m = rea_som()$map
      plot(m, type="changes")
      
    })
    
    , renderPlot({
      
      m = rea_som()$map
      plot(m, type="counts", shape = 'straight')
      
    }, width = 1024, height = 768)
    
    , renderPlot({
      
      m = rea_som()$map
      plot(m, type="dist.neighbours", shape = 'straight')
    }, width = 1024, height = 768)
    
    , renderPlot({
      
      m = rea_som()$map
      plot(m, type="quality", shape = 'straight')
    }, width = 1024, height = 768)
    
    , renderUI({
      
      selectInput('codes'
                  , label = 'Select Map Layer'
                  , choices = names( rea_som()$m$codes )
      )
      
    })
    
    , renderPlot({
      
      m = rea_som()$m
      
      plot(m
           , type = 'codes'
           , whatmap = input$codes
           , shape = 'straight'
      )
      
    }, width = 1024, height = 768 )
    
  )
  
  
}