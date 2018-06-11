
require(shiny)
require(tidyverse)


mod_summary_ui = function(rea_clean){
  
  shiny::tagList(
    
      h4('Sample View (100 random observations)')
      , DT::renderDataTable( options = list( pageLength = 5)
                           
         ,{
           
           withProgress(message = 'Rendering Summary Table'
                        ,{
           
           
             d_clean = rea_clean() 
             
             validate( need(!is.null(d_clean$data), 'Press LOAD DATA and CLEAN DATA') )
  
             data  = d_clean$data %>%
               sample_n(100)
             
             all_variables = d_clean$all_variables
           
          })
           
           return( data[ ,all_variables] )
           
         })
      
    ,h4('Numerics')
  
    ,renderPrint({
      
      withProgress(message = 'Printing Numerics Summary'
                   ,{
                     
        d_clean = rea_clean()
        
        data          = d_clean$data
        numericals    = d_clean$numericals
      
      })
      
      print(summary( data[, numericals]))
      
      
    })
    
    ,h4('Categoricals')
    
    , renderPrint({
      
      withProgress(message = 'Printing Categoricals Summary'
                   ,{
      
        d_clean = rea_clean()
        
        data          = d_clean$data
        categoricals    = d_clean$categoricals
      
      })
      
      print(summary( data[, categoricals]))
      
    })
    
    ,h4('Missing Values')
    
    , renderPlot({
      
      withProgress(message = 'Plotting Missmap'
                   ,{
      
        d_clean = rea_clean()
        
        data             = d_clean$data
        all_variables    = d_clean$all_variables
        
        p = Amelia::missmap( as_data_frame(data)[, all_variables])
      
      })
      
      return(p)
    })
    
  
  )
}