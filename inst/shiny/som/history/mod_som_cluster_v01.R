
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
                               , rea_som
                               , rea_dist
                               ){
  
  eventReactive( input$but_clust
                 , {
    
    withProgress( message = 'Performing Clustering'
                  ,{
    
      dist_adj      = rea_dist()
      map           = rea_som()$map
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
      
      #remove map_loc from data
      
      data_pred = data_pred %>%
        select(- map_loc)
      
      #add cluster to categoricals
      categoricals = c(categoricals, 'cluster')
      
      # we have to pack a new dataobject to pass on
      data_new = list(data           = data_pred
                      , numericals   = numericals
                      , categoricals = categoricals
      )
      
      som_clust = list(data = data_new
                       , clust = som_cluster_adj)
      
      status$clust = 'Clustering Done'
      
      return(som_clust)
    
    })
    
  })
  
  
}

mod_som_cluster_plot = function(input, rea_clust, rea_som){
  
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
      
      #this code is a bit ugly, i stole it
      #defines a color pallette
      
      qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
      pallette = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
      
      
      p = plot(map
               , type="codes"
               , main = "Clusters"
               , bgcol = pallette[clust]
               , pchs = NA
               , whatmap = input$codes_clust
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




