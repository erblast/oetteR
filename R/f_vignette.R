
#' @title returns vignette path
#' @examples
#' \dontrun{
#'  f_vign_get_path()
#' }
#' @rdname f_vign_render
#' @export
f_vign_get_path = function(){

  return( file.path( system.file(package = 'oetteR')
                     , 'vignettes') )
}

#' @title returns files of specified type from vignette path
#' @param file_type character vector denoting file extensions
#' @examples
#' \dontrun{
#'  f_vign_get_file_type()
#' }
#' @rdname f_vign_render
#' @export
f_vign_get_file_type = function( file_type = 'html' ){

  path_vign = f_vign_get_path()

  files = dir(path_vign)

  files_filt = files[ endsWith(files, paste0( '.', file_type ) ) ] %>%
    str_split(  '\\.') %>%
    map( function(x) x[1])

  return(files_filt)

}

#' @title renders vignettes
#' @param overwrite logical, Default: F
#' @examples
#' \dontrun{
#'  f_vign_render()
#' }
#' @rdname f_vign_render
#' @export
f_vign_render = function( overwrite = F ){

  path_vign = f_vign_get_path()

  rmds = f_vign_get_file_type( 'Rmd' )
  htmls = f_vign_get_file_type( 'html' )

  if( overwrite ){
    render = rmds
  }else{
    render = rmds[ ! rmds %in% htmls ]
  }

  if( ! is_empty(render) ){

    render %>%
      map( function(x) file.path( path_vign, paste0(x, '.Rmd') ) ) %>%
      walk( rmarkdown::render )

    }
}


#' @title open all vignettes
#' @description opens all oetteR vinettes in default browser, renders vignettes if they are not rendered yet.
#' @param vignettes vector with filenames of vignettes without file extensions, Default: f_vign_get_file_type("html")
#' @param render_missing logical, Default: T
#' @examples
#' \dontrun{
#'  f_vignettes()
#' }
#' @rdname f_vignettes
#' @export
f_vignettes = function( vignettes = f_vign_get_file_type('html')
                   , render_missing = T ){

  if(render_missing) f_vign_render(overwrite = F)

  vignettes %>%
    map( function(x) file.path( f_vign_get_path(), paste0(x, '.html') ) ) %>%
    walk( browseURL )

}









