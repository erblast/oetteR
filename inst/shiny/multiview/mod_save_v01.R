


mod_save = function(input
                     , rea_data
                      ){
  
  

  f_save = function(file){
    
    withProgress( message = 'saving Data'
                  ,{
                    
                    data = rea_data()$data %>%
                      select( one_of(input$save_cols) )
                    
                    save( data, file = file)
                    
                  })
    
  }
  
  #in a markdown document no downloadButton needs to be
  #created. outputArgs takes kwargs for the layout 
  #of the button, however the outputID is aumatically
  #assigned in this case dont pass it in outputArgs.
  
  tagList(
    
    inputPanel(
      
      selectInput( 'save_cols_preselect'
                     , 'Preselect Columns'
                     , choices = c('All', 'None') )
      
      , downloadHandler( filename = 'multiview_save'
                      , content = f_save
                      , outputArgs = list(label = 'Save Data'))
      
      
    )
    
    , wellPanel(
      
      selectInput('save_server'
                    ,'Select Server'
                    , choices = c('prod', 'dev', 'test'))
      
      , textInput('save_schema'
                  , 'Schema Name (Saves in jemas_temp)'
                  , value = 'dbo' )
      
      , textInput('save_table_name'
                  , 'Table Name'
                  , value = 'multiview_save' )
      
      
      , checkboxInput('save_overwrite'
                      , 'Overwrite')
      
      , actionButton( 'but_save_db'
                    ,'Save Data to DB')
      
     
    )
    
    , renderUI({
      
      data = rea_data()$data
      cols = names(data)
      preselect = input$save_cols_preselect
      
      if(preselect == 'All'){
        
        preselect = cols
        
      } else{
        
        preselect = NULL
        
      }
      
      inputPanel(
        
        checkboxGroupInput('save_cols'
                           , 'Select columns to save'
                           , choices = cols
                           , selected = preselect)
        
      )
      
    })
  ) 
    # observeEvent( input$but_save_db
    #               ,{
    #   withProgress(message = 'Saving to DB'
    #                ,{
    #                  
    #    # TODO enter jemas_write function
    #    })
    #   
    # })
}
