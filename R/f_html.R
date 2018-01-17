


#' @title create a taglist with n lines of html line breaks
#' @param n number of line breaks
#' @return taglist
#' @examples
#' f_html_breaks(5)
#' @seealso
#'  \code{\link[htmltools]{br}},\code{\link[htmltools]{tagList}}
#' @rdname f_html_breaks
#' @export
#' @importFrom htmltools br tagList
f_html_breaks = function(n){

  if ( n == 0 ) return( NULL )

  l = list()

  for( i in 1:n ){

    l = list( l, htmltools::br() )
  }

    return( htmltools::tagList(l) )
}


#' @title add some padding around html objects
#' @param obj html object such as DT:datatable() or plotly::ggplotly()
#' @param pad_before integer, Default: 0
#' @param title character vector, Default: ''
#' @param subtitle character vector, Default: ''
#' @param caption character vector, Default: ''
#' @param pad_after character vector, Default: 0
#' @return taglist
#' @examples
#' f_html_padding(DT::datatable(mtcars),5,'mtcars Data','subtitle', 'caption', 8 )
#' @seealso
#'  \code{\link[htmltools]{tagList}},\code{\link[htmltools]{h3}},\code{\link[htmltools]{h4}},\code{\link[htmltools]{h6}}
#' @rdname f_html_padding
#' @export
#' @importFrom htmltools tagList h3 h4 h6
f_html_padding = function( obj
                           , pad_before = 0
                           , title = NULL
                           , subtitle = NULL
                           , caption = NULL
                           , pad_after = 0 ){

  if( is.null(obj) | is_empty(obj) ){
    return()
  }

  l = list( f_html_breaks(pad_before) )

  if( ! is.null(title) ){
    l = f_manip_append_2_list( l, htmltools::h3(title) )
  }

  if( ! is.null(subtitle) ){
    l = f_manip_append_2_list( l, htmltools::h4(subtitle) )
  }

  l = f_manip_append_2_list( l, obj )

  if( ! is.null(caption) ){
    l = f_manip_append_2_list( l, htmltools::h6(caption) )
  }

  l = f_manip_append_2_list( l, f_html_breaks(pad_after) )


  return( htmltools::tagList(l) )
}

#' @title convert a filename + path or a file_path  to a html link
#' @param file_name character vector, Default: dir()[1]
#' @param path character vector, Default: getwd()
#' @param file_path file.path( path, file_name)
#' @param link_text character vector
#' @return link
#' @examples
#' dir()[1]
#' f_html_filename_2_link()
#' dir()[1:5]
#' f_html_filename_2_link(dir()[1:5])
#'
#' @seealso
#'  \code{\link[stringr]{str_replace_all}}
#' @rdname f_html_filename_2_link
#' @export
#' @importFrom stringr str_replace_all
f_html_filename_2_link = function( file_name = dir()[1]
                                     , path = getwd()
                                     , file_path = NULL
                                     , link_text = file_name
                                     ){

  if( is.null(file_path) ){
    path = file.path( path, file_name ) %>%
      stringr::str_replace_all( ' ', '%20') %>%
      unlist()
  }else{
    path = file_path %>%
      stringr::str_replace_all( ' ', '%20') %>%
      unlist()
  }

  link =  paste0('<a target=_blank href=', path, '>', link_text,'</a>' )

  return(link)
}



