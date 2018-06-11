
require(shiny)

mod_som_cluster_dist = function(rea_som){
  
  reactive({
  
    m    = rea_som()$map
    grid = rea_som()$grid
    
    # fuse all layers into one dataframe
    codes = tibble( layers = names(m$codes)
                    ,codes = m$codes ) %>%
      mutate( codes = purrr::map(codes, as_tibble) ) %>%
      spread( key = layers, value = codes) %>%
      apply(1, bind_cols) %>%
      .[[1]] %>%
      as_tibble()
    
    # generate distance matrix for codes
    dist_m = dist(codes) %>%
      as.matrix()
    
    # generate seperate distance matrix for map location
    
    dist_on_map = kohonen::unit.distances(grid)
    
    
    #multiply euclidean distance by distance on map
    dist_adj = dist_m ^ dist_on_map
  
    return(dist_adj)
    
  })
}


mod_som_cluster_opt_no_clust = function( input, rea_dist){
  
  tagList(
    
    numericInput('max_no_clust'
                , 'Select Maximum No of Clusters'
                , min = 2
                , max = 100
                , value = 15)
    
    , renderText('Ellbow Method')
    
    ,renderPlot({
      
      withProgress(message = 'Applying Ellbow method'
                   ,{
      
        #ellbow method
        factoextra::fviz_nbclust(rea_dist()
                                 , factoextra::hcut
                                 , method = "wss"
                                 , hc_method = 'ward.D2'
                                 , k.max = input$max_no_clust) 
      })
      
    })
    
    , renderText('Sillhouette Method')
    
    , renderPlot({
      
      withProgress(message = 'Applying Sillhouette method'
                   ,{
                     
      #silhouette method
      factoextra::fviz_nbclust(rea_dist()
                               , factoextra::hcut
                               , method = "silhouette"
                               , hc_method = "ward.D2"
                               , k.max =  input$max_no_clust)
      })
      
    })
  )
  
}

mod_som_cluster_ui = function(input){
  
  renderUI({
    
    inputPanel(
    
    numericInput('no_clust'
                 , 'Select No of Clusters'
                 , min = 2
                 , max = input$max_no_clust
                 , value = 2)
    
    , actionButton('but_clust'
                   ,'Start Clustering'
                   )
    
    )
    
  })
  
}

mod_som_cluster_rea = function(input
                               , status
                               , store
                               , rea_som
                               , rea_dist
                               ){
  
  eventReactive( input$but_clust
                 , {
    
    withProgress( message = 'Performing Clustering'
                  ,{
      
      store$assign_clust = NULL
      store$map_loc      = NULL
      
      dist_adj      = rea_dist()
      map           = rea_som()$map
      grid          = rea_som()$grid
      data          = rea_som()$data$data
      numericals    = rea_som()$data$numericals
      categoricals  = rea_som()$data$categoricals
      
      #perform hierarchical clustering
      clust_adj = hclust(as.dist(dist_adj), 'ward.D2')
      
      #cut tree at desired number of clusters
      som_cluster_adj = cutree(clust_adj, input$no_clust)
      
      #link cluster association to original cleaned dataframe
      link = tibble( map_loc = names(som_cluster_adj) %>% as.integer()
                     ,cluster = som_cluster_adj %>% as.factor() )
      
      pred = tibble( map_loc = map$unit.classif) %>%
        left_join(link)
      
      data_pred = data %>%
        bind_cols(pred)
      
      # #remove map_loc from data
      # 
      # data_pred = data_pred %>%
      #   select(- map_loc)
      
      #add cluster to categoricals
      categoricals = c(categoricals, 'cluster')
      
      # we have to pack a new dataobject to pass on
      data_new = list(data           = data_pred
                      , numericals   = numericals
                      , categoricals = categoricals
      )
      
      som_clust = list(data = data_new
                       , clust = som_cluster_adj
                       , grid  = grid)
      
      status$clust = 'Clustering Done'
      
      return(som_clust)
    
    })
    
  })
  
  
}


mod_som_cluster_man_change_cluster_UI = function( input ){
  
  renderUI({
  
    inputPanel(
      
      numericInput('x_cord'
                   , 'Select x Coordinate'
                   , min   = 1
                   , max   = input$map_dim_a
                   , value = 1
                   )
      , numericInput('y_cord'
                     , 'Select y coordinate'
                     , min   = 1
                     , max   = input$map_dim_b
                     , value = 1
                    )
      , numericInput('assign_clust'
                     , 'Cluster No to assign'
                     , min   = 1
                     , max   = 100
                     , value = 1
                     )
      , actionButton( 'but_assign_clust'
                      , 'Assign cluster to coordinate')
    )
  
  })
}

mod_som_store_rea = function(){
  
  reactiveValues( data = NULL )
  
}

mod_som_cluster_man_change_cluster_rea = function( input, store, rea_clust){
  
  eventReactive({
                  input$but_assign_clust
                  input$but_clust
                },{
    
    # we dont want to make changes if but_clust is pressed
    # only when but_assign_clust is pressed      
    
    if( is.null(store$but_clust) ) store$but_clust = 0              
                  
    if(  input$but_assign_clust == 0 | input$but_clust > store$but_clust ){
      
      store$but_clust = as.integer( input$but_clust )
      return( rea_clust() )
    }             
    
    # cluster info is stored in data as sperate column in dataframe
    # and as named int in clust, both need to be changed
                  
    clust_ls = rea_clust()              
    data_ls  = clust_ls$data
    data     = data_ls$data
    clust    = clust_ls$clust
    grid     = clust_ls$grid
    
    # for some reason maploc == 1 is the bottom left corner of the visual map
    # we need to transform the coordinates appropriately
    map_loc = grid$xdim * ( grid$ydim - input$y_cord ) + input$x_cord
    
    # we need to store cluster assignments and reapply them each time this 
    # function is called, otherwise we loose them because we always start with 
    # the freshly clustered data from rea_clust
    
    store$map_loc      = c( store$map_loc, map_loc )
    store$assign_clust = c( store$assign_clust, input$assign_clust )
    
    
    for( i in 1 : length(store$assign_clust) ) {

      # we have to assign the cluster in two different locations for storage
      # data is used but for the map colorisation clust is used
      
      data = data %>%
        mutate( cluster = ifelse( map_loc == store$map_loc[i], store$assign_clust, cluster ) )
      
      clust[ store$map_loc[i] ] = store$assign_clust
      
    }
    
    data_ls$data = data
    
    clust_ls$data  = data_ls
    clust_ls$clust = clust
    
    return( clust_ls )
                  
  }, ignoreNULL = F, ignoreInit = T)
  
}


mod_som_cluster_plot = function(input, color_pallette, rea_clust, rea_som){
  
  tagList(
  
    renderUI({
      
      selectInput('codes_clust'
                  , label = 'Select Map Layer'
                  , choices = names( rea_som()$m$codes )
      )
      
    })
    
    
    , renderPlot({
      
      map   = rea_som()$map
      clust = rea_clust()$clust
      whatmap = input$codes_clust
      
      if( length(whatmap) == 1 ){
        whatmap = NULL
      }
      

      p = plot(map
               , type="codes"
               , main = "Clusters"
               , bgcol = color_pallette[clust]
               , pchs = NA
               , whatmap = whatmap
               , shape = 'straight'
               )
    
  
    }, width = 1024, height = 768)
  
  )
}
  



mod_som_clust_2_data = function(rea_clust){
  
  reactive({
    rea_clust()$data
  })
}




