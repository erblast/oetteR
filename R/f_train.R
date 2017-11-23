
#' @title container for function calls, can be used as a progress bar
#' @description creates a closure with a make_call() method that wraps
#' any function call. When the wrapper is used the function call is saved
#' and the calls are counted and the progress is being printed. Use the method
#' set_total() to input the total number of function calls. Based on the total
#' an ETA is estimated and a percentage calculated.

#' @return container
#' @details DETAILS
#' @examples
#'
#' .f = randomForest::randomForest
#' call_cont = make_container_for_function_calls()
#' call_cont$set_total(4)
#' m_wr = call_cont$make_call( .f = .f, formula = disp~., data = mtcars )
#'
#' #pipe version
#' call_cont = make_container_for_function_calls()
#' call_cont$set_total(5)
#'
#' pl = pipelearner::pipelearner(mtcars) %>%
#'   pipelearner::learn_models( models = c( call_cont$make_call )
#'                              , formulas = c(disp~.)
#'                              , .f = c( randomForest::randomForest )
#'                              , print_call = c(T)
#'                            ) %>%
#'   pipelearner::learn_cvpairs( pipelearner::crossv_kfold, k = 5 ) %>%
#'   pipelearner::learn()
#'
#' @seealso
#'  \code{\link[lubridate]{now}},\code{\link[lubridate]{time_length}}
#' @rdname make_container_for_function_calls
#' @export
#' @importFrom lubridate now time_length
make_container_for_function_calls = function(){

  #attributes ----------------------------

  total      = NULL
  calls      = list()
  counter    = 1
  time_start = NULL
  time_end   = NULL

  #methods -------------------------------

  set_total = function(total){

    total <<- total

  }

  get_call = function(call){

    call = as.character(call)
    call = call[ ! startsWith(call, 'list')]
    call = call[ ! startsWith(call, 'function')]
    call = paste( call, collapse = ' ; ')

  }

  make_call = function( .f
                        , print_progress = T
                        , print_call = F
                        , ... ){

    if( counter == 1) time_start <<- lubridate::now()

    x = .f(...)

    if( 'call' %in% names(x) ){
      call = paste( get_call( match.call() ), get_call(x$call), collapse = ' ; ' )
    }else{
      call = get_call( match.call() )
    }

    calls <<- f_manip_append_2_list( calls, call )

    if( print_call ) print( call )

    perc               = round( counter / total * 100, 1 )
    time_elapsed_s     = (lubridate::now() - time_start) %>% lubridate::time_length()
    time_predicted_s   = time_elapsed_s/counter * (total - counter)
    time_elapsed_min   = round(time_elapsed_s   / 60, 1)
    time_predicted_min = round(time_predicted_s / 60, 1)

    if(print_progress) print( paste( 'Progress:', counter, '/', total, ';'
                                     , round(perc,1), '%', ';'
                                     , 'ETA:', time_predicted_min, 'min'
                                     )
                              )


    if( counter == total ) time_end <<- lubridate::now()

    counter <<- counter + 1

    return( x )

  }

  get_calls = function(){
    return(calls)
  }

  get_total = function(){
    return(total)
  }

  get_counter = function(){
    return(counter)
  }

  get_run_time = function(){

    return( time_start - time_end )

  }


  #returns--------------------------------

  return( list( set_total = set_total
                , make_call  = make_call
                , get_total = get_total
                , get_calls = get_calls
                , get_counter = get_counter
               )
         )


}


