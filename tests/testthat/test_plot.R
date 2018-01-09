
context('Plot functions')

test_that('plot histogram'
          ,{

  data_ls = f_clean_data(mtcars)

  f_plot_hist('disp', data_ls, add = 'median')
  f_plot_hist('disp', data_ls, add = 'none')
  f_plot_hist('disp', data_ls)
  f_plot_hist('disp', data_ls, y_axis = 'density')
  f_plot_hist('cyl', data_ls , group = 'gear' )
  f_plot_hist('cyl', data_ls , group = 'gear', y_axis = 'density' )
  f_plot_hist('cyl', data_ls, y_axis = 'density' )
  f_plot_hist('cyl', data_ls, y_axis = 'count' )
  f_plot_hist('disp', data_ls, graph_type = 'line', group = 'cyl')
  f_plot_hist('disp', data_ls, graph_type = 'bar', group = 'cyl')
  f_plot_hist('disp', data_ls, graph_type = 'violin', group = 'cyl'
              , caption ='caption', title = 'title', subtitle = 'subtitle')

})

test_that('plot time'
          ,{

    set.seed(1)
    data       = dplyr::sample_n( nycflights13::flights, 1000 )
    data$is_ua = ifelse( data$carrier == 'UA', 'UA', 'other')
    data$date  = data$year * 10000 + data$month * 100 + data$day
    data$date  = lubridate::as_date( data$date )
    data_ls    = f_clean_data( data, replace_neg_values_with_zero = F)

    expect_is( f_plot_time( 'arr_delay'
                            , 'month'
                            , data_ls
                            , group = 'is_ua'
                            , time_unit = 'month'
                            , time_variable_as_factor = T
                            ), 'ggplot' )

    expect_is( f_plot_time( 'arr_delay'
                            , 'month'
                            , data_ls
                            , time_unit = 'month'
                            , time_variable_as_factor = F
                            ), 'ggplot' )

})

test_that('color scale'
          ,{

  expect_true( length( f_plot_col_vector74() ) == 74 )
  expect_true( length( f_plot_adjust_col_vector_length(100) ) == 100 )
  expect_true( length( f_plot_adjust_col_vector_length(20) ) == 20 )

})


test_that( 'f_plot_color_code_variables',{

  data_ls = f_clean_data(mtcars)

  col1 = f_plot_color_code_variables( data_ls )
  col2 = f_plot_color_code_variables( data_ls )

  expect_identical(col1, col2)

})


test_that('f_plot_obj_2_html'
          ,{


  file_name_template = 'taglist_2_html_template.Rmd'

  path_template = file.path( system.file(package = 'oetteR')
                             , 'templates'
                             , file_name_template)

  # if package is not installes only loaded all files from ./inst
  # will not yet have been moved to the parent installation directory
  if( ! file.exists(path_template) ){
    path_template = file.path( system.file(package = 'oetteR')
                               , 'inst'
                               , 'templates'
                               , file_name_template)
  }

  # print(getwd())
  # print(path_template)

  expect_true( file.exists( path_template ) )

  # htmltools::taglist ----------------------------------------------
  #returns a htmltools::taglist with DT::datatables and plotly plots
  taglist = f_clean_data(mtcars) %>%
    f_boxcox() %>%
    f_pca() %>%
    f_pca_plot_components()

  f_plot_obj_2_html(taglist, type = "taglist", output_file =  'test_me', title = 'Plots')
  file.remove('test_me.html')

  # list( tabplot::tableplot ) ---------------------------------------
  form = as.formula('disp~cyl+mpg+hp')
  pipelearner::pipelearner(mtcars) %>%
    pipelearner::learn_models( twidlr::rpart, form ) %>%
    pipelearner::learn_models( twidlr::randomForest, form ) %>%
    pipelearner::learn_models( twidlr::svm, form ) %>%
    pipelearner::learn() %>%
    dplyr::mutate( imp = map2(fit, train, f_model_importance)
            , tabplot = pmap( list( data = train
                                   , ranked_variables = imp
                                   , response_var = target
                                   , title = model
                                   )
                           , f_model_importance_plot_tableplot
                           , limit = 5
                           )
    )  %>%
    .$tabplot %>%
    f_plot_obj_2_html( type = "tabplots", output_file =  'test_me', title = 'Plots')


    file.remove('test_me.html')


    #list(ggplot2)---------------------------------------------------
    data_ls = f_clean_data(mtcars)
    form = as.formula('disp~cyl+mpg+hp')
    variable_color_code = f_plot_color_code_variables(data_ls)

    pipelearner::pipelearner(data_ls$data) %>%
      pipelearner::learn_models( twidlr::rpart, form ) %>%
      pipelearner::learn_models( twidlr::randomForest, form ) %>%
      pipelearner::learn_models( twidlr::svm, form ) %>%
      pipelearner::learn() %>%
      dplyr::mutate( imp = map2(fit, train, f_model_importance)
              , tabplot = pmap( list( m = fit
                                      , ranked_variables = imp
                                      , title = model
                                      )
                                , f_model_plot_variable_dependency_regression
                                , formula = form
                                , data_ls = data_ls
                                , variable_color_code = variable_color_code
              )
      )  %>%
      .$tabplot %>%
      f_plot_obj_2_html( type = "plots", output_file =  'test_me', title = 'Plots')

    file.remove('test_me.html')

    #list(grids)---------------------------------------------------

    data_ls = f_clean_data(mtcars)
    form = as.formula('disp~cyl+mpg+hp+am+gear+drat+wt+vs+carb')
    variable_color_code = f_plot_color_code_variables(data_ls)

    grids = pipelearner::pipelearner(data_ls$data) %>%
      pipelearner::learn_models( twidlr::rpart, form ) %>%
      pipelearner::learn_models( twidlr::randomForest, form ) %>%
      pipelearner::learn_models( twidlr::svm, form ) %>%
      pipelearner::learn() %>%
      dplyr::mutate( imp = map2(fit, train, f_model_importance)
                     , range_var = map_chr(imp, function(x) head(x,1)$row_names )
                     , grid = pmap( list( m = fit
                                         , title = model
                                         , variables = imp
                                         , range_variable = range_var
                                         , data = test
                                         )
                                 , f_model_plot_var_dep_over_spec_var_range
                                 , formula = form
                                 , data_ls = data_ls
                                 , variable_color_code = variable_color_code
                                 , log_y = F
                                 , limit = 12
                                 )
      )  %>%
      .$grid

      f_plot_obj_2_html( grids,  type = "grids", output_file =  'test_me', title = 'Grids', height = 30 )

    file.remove('test_me.html')

})


test_that( 'plot_profit_lines'
           ,{

  data = tibble( time     = c(0,1,2,3,4,5,6,7,8,9,10)
                , revenue = - time^2 + time * 12
                , cost    = revenue * 0.4 * -1
                )

  print( f_plot_profit_lines( data, 'revenue', 'cost', 'time') )
  print( f_plot_profit_lines( data, 'revenue', 'cost', 'time', now = 5) )

})

test_that( 'pretty points'
           ,{

    df = ggplot2::diamonds %>%
      sample_n(2500)
    col_x = 'carat'
    col_y = 'price'
    col_facet = 'cut'
    title = 'diamonds'

    f_plot_pretty_points(df, col_x, col_y, col_facet, title = title)

    df$carat = 1
    df$price = 2

    f_plot_pretty_points(df, col_x, col_y, col_facet, title = title)

    f_plot_pretty_points(df, col_x, col_y, col_facet = NULL, title = title)

})

