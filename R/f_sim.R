


#' @title simulate profit
#' @description using the following parameters:
#'  \itemize{
  #'  \item retention rate
  #'  \item new customers acquired per year
  #'  \item present number of customers
  #'  \item fixed cost
  #'  \item profit per customer per year
#'  }
#' @param output_file PARAM_DESCRIPTION, Default: 'profit_simulation'
#' @param path PARAM_DESCRIPTION, Default: '.'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  f_sim_profit( path = tempdir() )
#'  }
#' }
#' @seealso \code{\link[rmarkdown]{render}}
#' @rdname f_sim_profit
#' @export
#' @importFrom rmarkdown render
f_sim_profit = function( output_file = 'profit_simulation'
                         , path = '.'){

  path_rmd = file.path( system.file(package = 'oetteR')
                        , 'Rmd examples' )

  file_rmd = file.path( path_rmd, 'example_portfolio_development.Rmd' )

  output_file_html = paste0( output_file, '.html')

  rmarkdown::render( file_rmd
                     , output_file = output_file_html
                     , params      = 'ask'
  )

  file.copy( file.path( path_rmd, output_file_html)
             , file.path( '.', output_file_html) )

  file.remove( file.path( path_rmd, output_file_html ) )

}
