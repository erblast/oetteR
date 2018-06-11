

f_bcag_multiview = function(data){
  
  file_name = 'multiview_v07.Rmd'
  
  Sys.setenv(RSTUDIO_PANDOC="//jes1192/c$/Program Files/RStudio/bin/pandoc")
  print( Sys.getenv("RSTUDIO_PANDOC") )
  
  options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
  
  path = c("\\\\JESF01/DocumentH/Sales and Marketing/60_Customer Analytics/01_Team/10_Einarbeitung/06_Oettinghaus_B/multiview")
  
  rmarkdown::run( paste0( path, '/', file_name)
                 , shiny_args= list(launch.browser = T)
                 , auto_reload = F
                 , default_file = file_name
                 , render_args = list( params = list(  data       = data
                                                     , data_input = F
                                                     ) 
                                       )
  )
  
  
}