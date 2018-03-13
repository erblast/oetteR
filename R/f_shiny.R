



#' @title run multiview shiny app
#' @description shiny tool to quickly analyze diffferences between groups in
#'   large datasets
#' @param data dataframe, will let you choose between a variety of sample data
#'   sets if NULL, Default: NULL
#' @seealso \code{\link[rmarkdown]{run}}
#' @rdname f_shiny_multiview
#' @export
#' @importFrom rmarkdown run
f_shiny_multiview = function(data = NULL){

  path = file.path( system.file(package = 'oetteR'), 'shiny', 'multiview' )

  file = 'multiview_v08.Rmd'

  file_path  = file.path(path, file)

  if( ! all( c(dir.exists(path), file.exists(file_path)) ) ){

    stop('Multiview Shiny not found in oetteR installation')

  }

  # this lets you select a sample dataset
  if( is.null(data) ){
    data_input = TRUE
  }else{
    data_input = FALSE
  }

  rmarkdown::run(file = file_path
                 , dir = path
                 , auto_reload = F
                 , default_file = file
                 , shiny_args  = list( launch.browser = T)
                 , render_args = list( params = list( data = data
                                                      , data_input = data_input
                 )
                 )
  )

}



#' @title self organizing map shiny app
#' @description shiny tool to build self organizing maps with subsequent
#'   clustering restricted to adjacent map segements. See http://rpubs.com/erblast/SOM.
#' @param data dataframe, will let you choose between a variety of sample data
#'   sets if NULL, Default: NULL
#' @seealso \code{\link[rmarkdown]{run}}
#' @rdname f_shiny_som
#' @export
#' @importFrom rmarkdown run
f_shiny_som = function(data = NULL){

  path = file.path( system.file(package = 'oetteR'), 'shiny', 'som' )

  file = 'multiview_som_v03.Rmd'

  file_path  = file.path(path, file)

  if( ! all( c(dir.exists(path), file.exists(file_path)) ) ){

    stop('SOM Shiny not found in oetteR installation')

  }

  # this lets you select a sample dataset
  if( is.null(data) ){
    data_input = TRUE
  }else{
    data_input = FALSE
  }

  rmarkdown::run(file = file_path
                 , dir = path
                 , auto_reload = F
                 , default_file = file
                 , shiny_args  = list( launch.browser = T)
                 , render_args = list( params = list( data = data
                                                      , data_input = data_input
                 )
                 )
  )

}
