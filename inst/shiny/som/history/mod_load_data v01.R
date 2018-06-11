

require(shiny)
require(tidyverse)

mod_load_data_ui = function(){
  
  tagList(
  
  
    wellPanel(
      
      selectInput('sql_or_lib'
                  , label = 'Select Data Source'
                  , choices = c('SQL', 'Sample_Data')
                  , selected = 'Sample_Data')
      
      , selectInput('sample_data'
                    , label = 'Select Sample Data'
                    , choices = c('ISLR::OJ'
                                  , 'ISLR::Caravan'
                                  , 'ISLR::Auto'
                                  , 'ISLR::Carseats'
                                  , 'ISLR::Wage'
                                  , 'ISLR::College'
                                  , 'ISLR::Hitters'
                                  , 'ISLR::Default'
                                  , 'ISLR::Weekly'
                                  , 'ILSR::Wage categoricals'
                                  , 'ISLR::OJ numericals'
                                  , 'data diamonds')
                    , selected = 'ISLR::Auto')
      
      # , textInput('server'
      #             , label = 'Enter Server'
      #             , width = 'auto')
      # 
      # , textInput('query'
      #             , label = 'Paste SQL'
      #             , width = '100%')
      
      
      , textInput('path'
                  , label = 'Save Analysis to this directory'
                  , width = '100%'
                  , value = "C:/Users/erbla/Documents")
      )
    
    ,inputPanel(
      
      actionButton('load_data'
                   , label = 'Load Data')
      
      
    )
  )
}




mod_load_rea = function(input){

  rea_load = eventReactive(input$load_data
                           , ignoreNULL  = FALSE
                           , ignoreInit  = FALSE
                           ,{
  
    if(input$sql_or_lib == 'SQL'){
      
      require(RODBCext)
      
      con = odbcDriverConnect(
        str_c('Driver=SQL Server;Trusted_Connection=Yes;Server=', input$server)
      )
      
      query = input$query
      
      data = sqlQuery(con, query = query)
      
      close(con)
      
    }  
    
    else{
      
      if(input$sample_data == 'ILSR::Wage categoricals'){
        
        data = ISLR::Wage
        
        bool = ! summarise_all(data, is.numeric)
        
        data= data[,bool]
        
      }else if(input$sample_data == 'ISLR::OJ numericals'){
        
        data = ISLR::OJ %>%
          as.data.frame()
        
        bool = summarise_all(data, is.numeric)
        
        
        data= data[,as.logical(bool[1,])]
        
      }else if(startsWith(input$sample_data,'data')) {
        
        set = input$sample_data %>%
          stringr::str_extract(' [A-Za-z0-9]+$')%>%
          stringr::str_trim() 
        
        exec_str = set %>%
          stringr::str_c('data(',.,')')
        
        eval(parse(text = exec_str) )
        
        exec_str = set 
        
        data = eval(parse(text = exec_str) )
        
      }else{
        exec_str = input$sample_data
        
        data = eval(parse(text = exec_str) )
      }
      
      
    }
    
    return(data)
      
  })

}



save_plot = function(plot
                     , path
                     , name
                     , yes
                     , scale = 1
                     , excel = T){
  
  require(tidyverse)
  
  if(yes == F) return()
  
  folder_name = lubridate::today() %>%
    as.character() %>%
    stringr::str_replace_all('-','')%>%
    stringr::str_c('_multiview')
  
  path = path %>%
    stringr::str_c('/',folder_name)
  
  dir.create(path)
  
  file_name_plot = path %>%
    stringr::str_c('/',name,'.png')
  
  file_name_excel = path %>%
    stringr::str_c('/',name,'.xls')
  
  # tabplot object should save with the same function
  
  
  safe_ggsave    = safely(ggsave)
  safe_tableSafe = safely(tabplot::tableSave)
  
  s = safe_ggsave(file_name_plot
                  , plot = plot
                  , scale = scale)
  
  if( !purrr::is_empty(s$error) ){
    
    print(s$error)
    s = safe_tableSafe(tab = plot
                       ,filename = file_name_plot
                       , scale = scale)
    
  }
  
  
  if( !purrr::is_empty(s$error) ){
    
    print(s$error)
    
    plot
    
    dev.copy(png, filename = file_name_plot
    )
    dev.off()
    
  }
  
  
  
  
  if(excel == T){
    coords = ggplot_build(plot)
    
    if(file.exists(file_name_excel)) {
      file.remove(file_name_excel)
    }
    
    purrr::pwalk(list( x = coords$data
                       , sheetName = as.character(1:length(coords$data) ) 
    )
    , xlsx::write.xlsx
    , file = file_name_excel
    , col.names = T
    , row.names = T
    , append = T
    )
    
  }
  
}