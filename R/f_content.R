
#' @title returns content path
#' @examples
#' \dontrun{
#'  f_content_get_path()
#' }
#' @rdname f_content_get_path
#' @export
f_content_get_path = function( folder = 'vignettes' ){

  return( file.path( system.file(package = 'oetteR')
                     , folder) )
}

#' @title returns files of specified type from vignette path
#' @param file_type character vector denoting file extensions
#' @examples
#' \dontrun{
#'  f_content_get_file_type()
#' }
#' @rdname f_content_get_file_type
#' @export
f_content_get_file_type = function( file_type = 'html'
                                 , folder = 'vignettes'){

  path_vign = f_content_get_path( folder = folder )

  files = dir(path_vign)

  files_filt = files[ endsWith(files, paste0( '.', file_type ) ) ] %>%
    str_split(  '\\.') %>%
    map( function(x) x[1])

  return(files_filt)

}

#' @title renders content
#' @param overwrite logical, Default: F
#' @examples
#' \dontrun{
#'  f_content_render()
#' }
#' @rdname f_content_render
#' @export
f_content_render = function( overwrite = F
                          , folder = 'vignettes'){

  path_vign = f_content_get_path( folder = folder )

  rmds = f_content_get_file_type( 'Rmd' , folder = folder)
  htmls = f_content_get_file_type( 'html' , folder = folder)

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


#' @title opens an html index file for additional content
#' @description opens a window in the default browser with links to all
#'   additional content files in oetteR, including vignettes and proof of
#'   concepts drafted as R and Rmd files. It includes the option of rendering
#'   all Rmd files into html. Which however can take around 30 minutes at least.
#'   You can find links to most of the additional content rendered as html on
#'   the github page of the package.
#' @param render_missing logical, Default: T
#' @examples
#' \dontrun{
#'  f_vignettes()
#' }
#' @rdname f_content
#' @export
f_content = function( render_missing = F, overwrite = T ){

  if(render_missing){

    f_content_render(overwrite = F, folder = 'Vignettes')
    f_content_render(overwrite = F, folder = 'POC_Rmd')

  }

  path_oetteR = system.file(package = 'oetteR')

  if( ! file.exists( file.path( path_oetteR, 'content.html') )
      | overwrite == TRUE ){

    rmarkdown::render( input = file.path( path_oetteR, 'content.Rmd')
                       , quiet = T)

  }

  browseURL( file.path( path_oetteR, 'content.html') )
}


