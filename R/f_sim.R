


#' @title simulate profit
#' @description using the following parameters:
#'  \itemize{
  #'  \item retention rate (retention_rate)
  #'  \item retention common for business (retention_rate_common)
  #'  \item new customers acquired per year (nca_per_year)
  #'  \item expected increase in customers acquired  (expected_increase_nca)
  #'  \item present number of customers (n_customers)
  #'  \item fixed cost (fix_cost)
  #'  \item profit per customer per year (profit_cm1_per_customer)
#'  }
#' @param output_file PARAM_DESCRIPTION, Default: 'profit_simulation'
#' @param path PARAM_DESCRIPTION, Default: '.'
#' @param params list params passed to rmarkdown::render(), see example, Default: 'ask'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  f_sim_profit( path = tempdir() )
#'  f_sim_profit( prefix, params = list( retention_rate = 0.82
#'                                      , retention_rate_common = 0.88
#'                                      , nca_per_year = 4000
#'                                      , expected_increase_nca = 2
#'                                      , n_customers = 150000
#'                                      , fix_cost = 20000000
#'                                      , profit_cm1_per_customer = 200 )
#'  }
#' }
#' @seealso \code{\link[rmarkdown]{render}}
#' @rdname f_sim_profit
#' @export
#' @importFrom rmarkdown render
f_sim_profit = function( output_file = 'profit_simulation'
                         , path = '.'
                         , params = 'ask'){

  path_rmd = file.path( system.file(package = 'oetteR')
                        , 'simulations' )

  file_rmd = file.path( path_rmd, 'simulation_portfolio_development.Rmd' )

  output_file_html = paste0( output_file, '.html')

  rmarkdown::render( file_rmd
                     , output_file = output_file_html
                     , params      = params
                     , envir = new.env()
  )

  file.copy( file.path( path_rmd, output_file_html)
             , file.path( '.', output_file_html)
             , overwrite = T)

  suppressMessages(
    file.remove( file.path( path_rmd, output_file_html ) )
  )

  browseURL( file.path( '.', output_file_html) )
}
