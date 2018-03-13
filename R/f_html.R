


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
#' @param .f_htitle function, Default: htmltools::h3
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
                           , pad_after = 0
                           , .f_htitle = htmltools::h3 ){

  if( is.null(obj) | is_empty(obj) ){
    return()
  }

  l = list( f_html_breaks(pad_before) )

  if( ! is.null(title) ){
    l = f_manip_append_2_list( l, .f_htitle(title) )
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
#'file_path = dir( getwd(), full.names=TRUE)[2]
#'file_path
#'f_html_filename_2_link(file_path = file_path)
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

    link_text = normalizePath(path) %>%
      str_split( '\\\\') %>%
      unlist() %>%
      .[length(.)] %>%
      str_split('\\.') %>%
      unlist()%>%
      .[1]
  }

  link =  paste0('<a target=_blank href=', path, '>', link_text,'</a>' )

  return(link)
}

#' @title get title from Rmd file
#' @description usefull if we want to provide additional iformation if we wat to
#'   link to an html file that was generates from a Rmd file.
#' @param file_path path to Rmd file
#' @return character vector
#' @seealso \code{\link[readr]{read_lines}}
#' @rdname f_html_get_title_from_Rmd
#' @export
#' @importFrom readr read_lines
f_html_get_title_from_Rmd = function( file_path ){

  line = readr::read_lines( file_path, skip = 1, n_max = 1)
  title = str_extract(line, '\\".+\\"')

}

#' @title create a DT::datatable that pairs Rmd and rendered html files.
#' @description Extracts the title from the Rmd file and generates one column
#'   with links to the Rmd and one column with links to the html files.
#' @param wd working directory, Default: getwd()
#' @param folder_Rmd path to folder with Rmd files relative to wd (use '.' for
#'   wd and '..' for parent directory of wd ), Default: getwd()
#' @param folder_html path to folder with html files relative to wd (use '.' for
#'   wd and '..' for parent directory of wd ), Default: getwd()
#' @return DT::datatable
#' @examples
#'
#' wd = file.path( system.file(package = 'oetteR') )
#' folder_Rmd = 'vignettes'
#' folder_html = 'vignettes'
#'
#' f_html_table_html_and_rmd_link( wd, folder_Rmd, folder_html )
#'
#' @rdname f_html_table_html_and_rmd_link
#' @export
f_html_table_html_and_rmd_link = function( wd = getwd()
                                         , folder_Rmd = getwd()
                                         , folder_html = getwd() ){

  Rmd = tibble( file = dir( file.path( wd, folder_Rmd ), pattern = '\\.Rmd', full.names = FALSE ) ) %>%
    mutate( path = dir( file.path( wd, folder_Rmd ), pattern = '\\.Rmd', full.names = TRUE )
            , title = map_chr( path, f_html_get_title_from_Rmd )
            , link =  map_chr( path, function(x) f_html_filename_2_link( file_path = x ) )
            , file_prefix = str_split(file, '\\.')
            , file_prefix = map_chr( file_prefix, function(x) x[[1]] )
    )

  html = tibble( file = dir( file.path( wd, folder_html ), pattern = '\\.html', full.names = FALSE ) ) %>%
    mutate( path = dir( file.path( wd, folder_html ), pattern = '\\.html', full.names = TRUE )
            , link =  map_chr( path, function(x) f_html_filename_2_link( file_path = x ) )
            , file_prefix = str_split(file, '\\.')
            , file_prefix = map_chr( file_prefix, function(x) x[[1]] )
    )


  table = Rmd %>%
    left_join( html, by = 'file_prefix', suffix = c('_Rmd', '_html') ) %>%
    select( title, file_prefix, link_Rmd, link_html) %>%
    rename( file = file_prefix ) %>%
    mutate_all( function(x) ifelse(is.na(x), 'missing', x) )


  f_datatable_minimal(table)


}



